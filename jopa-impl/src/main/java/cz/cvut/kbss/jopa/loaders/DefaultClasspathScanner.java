/*
 * JOPA
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */
package cz.cvut.kbss.jopa.loaders;

import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.net.URLDecoder;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.function.Consumer;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;

/**
 * Processes classes available to the current classloader.
 */
public class DefaultClasspathScanner implements ClasspathScanner {

    private static final Logger LOG = LoggerFactory.getLogger(DefaultClasspathScanner.class);

    protected static final char JAVA_CLASSPATH_SEPARATOR = '/';
    protected static final char WINDOWS_FILE_SEPARATOR = '\\';
    protected static final char JAVA_PACKAGE_SEPARATOR = '.';
    protected static final String JAR_FILE_SUFFIX = ".jar";
    protected static final String CLASS_FILE_SUFFIX = ".class";

    protected final List<Consumer<Class<?>>> listeners = new ArrayList<>();

    protected final ClassLoader classLoader = Thread.currentThread().getContextClassLoader();

    protected String pathPattern;
    protected Set<URL> visited;

    @Override
    public void addListener(Consumer<Class<?>> listener) {
        listeners.add(listener);
    }

    /**
     * Inspired by https://github.com/ddopson/java-class-enumerator
     */
    @Override
    public void processClasses(String scanPackage) {
        this.pathPattern = scanPackage.replace(JAVA_PACKAGE_SEPARATOR, JAVA_CLASSPATH_SEPARATOR);
        this.visited = new HashSet<>();
        try {
            Enumeration<URL> urls = classLoader.getResources(pathPattern);
            processElements(urls, scanPackage);
            // Scan jar files on classpath
            Enumeration<URL> resources = classLoader.getResources(".");
            processElements(resources, scanPackage);
        } catch (IOException e) {
            throw new OWLPersistenceException("Unable to scan packages for entity classes.", e);
        }
    }

    protected void processElements(Enumeration<URL> urls, String scanPath) throws IOException {
        while (urls.hasMoreElements()) {
            final URL url = urls.nextElement();
            if (visited.contains(url)) {
                continue;
            }
            visited.add(url);
            LOG.trace("Processing classpath element {}", url);
            if (isJar(url.toString())) {
                processJarFile(createJarFile(url));
            } else {
                processDirectory(new File(getUrlAsUri(url).getPath()), scanPath);
            }
        }
    }

    /**
     * Handles possible non-ascii character encoding in the specified URL.
     *
     * @param url Resource URL (presumably leading to a local file)
     * @return Decoded argument
     * @throws UnsupportedEncodingException Should not happen, using standard UTF-8 encoding
     */
    protected static String sanitizePath(URL url) throws UnsupportedEncodingException {
        return URLDecoder.decode(url.getFile(), StandardCharsets.UTF_8);
    }

    protected static boolean isJar(String filePath) {
        return filePath.startsWith("jar:") || filePath.endsWith(JAR_FILE_SUFFIX);
    }

    protected static JarFile createJarFile(URL elementUrl) throws IOException {
        final String jarPath = sanitizePath(elementUrl).replaceFirst("[.]jar/?!.*", JAR_FILE_SUFFIX)
                                                       .replaceFirst("file:", "")
                                                       .replaceFirst("nested:", "");
        return new JarFile(jarPath);
    }

    protected static URI getUrlAsUri(URL url) {
        try {
            // Transformation to URI handles encoding, e.g. of whitespaces in the path
            return url.toURI();
        } catch (URISyntaxException ex) {
            throw new OWLPersistenceException(
                    "Unable to scan resource " + url + ". It is not a valid URI.", ex);
        }
    }

    /**
     * Processes the specified {@link JarFile}, looking for classes in the configured package.
     *
     * @param jarFile JAR file to scan
     */
    protected void processJarFile(final JarFile jarFile) {
        LOG.trace("Scanning jar file {} for entity classes.", jarFile.getName());
        try (final JarFile localFile = jarFile) {
            final Enumeration<JarEntry> entries = localFile.entries();
            while (entries.hasMoreElements()) {
                final JarEntry entry = entries.nextElement();
                final String entryName = entry.getName();
                if (entryName.endsWith(CLASS_FILE_SUFFIX) && entryName.contains(pathPattern)) {
                    String className = entryName.substring(entryName.indexOf(pathPattern));
                    className = className.replace(JAVA_CLASSPATH_SEPARATOR, JAVA_PACKAGE_SEPARATOR)
                                         .replace(WINDOWS_FILE_SEPARATOR, JAVA_PACKAGE_SEPARATOR);
                    className = className.substring(0, className.length() - CLASS_FILE_SUFFIX.length());
                    processClass(className);
                }
            }
        } catch (IOException e) {
            throw new OWLPersistenceException("Unexpected IOException reading JAR File " + jarFile, e);
        }
    }

    /**
     * Retrieves a {@link Class} with the specified name and passes it to the registered listeners.
     *
     * @param className Fully-qualified class name
     */
    protected void processClass(String className) {
        try {
            final Class<?> cls = Class.forName(className, true, classLoader);
            listeners.forEach(listener -> listener.accept(cls));
        } catch (Exception | NoClassDefFoundError e) {
            LOG.debug("Unable to load class {}, got error {}: {}. Skipping the class. If it is an entity class, ensure it is available on classpath and is built with supported Java version.", className, e.getClass()
                                                                                                                                                                                                           .getName(), e.getMessage());
        }
    }

    /**
     * Processes the specified directory, looking for classes in the specified package (and its descendants).
     *
     * @param dir         Directory
     * @param packageName Package name
     */
    protected void processDirectory(File dir, String packageName) throws IOException {
        if (!dir.getPath().replace(WINDOWS_FILE_SEPARATOR, JAVA_CLASSPATH_SEPARATOR).contains(pathPattern)) {
            return;
        }
        LOG.trace("Scanning directory {} for entity classes.", dir);
        // Get the list of the files contained in the package
        final String[] files = dir.list();
        if (files == null) {
            return;
        }
        for (String fileName : files) {
            String className = null;
            // we are only interested in .class files
            if (fileName.endsWith(CLASS_FILE_SUFFIX)) {
                // removes the .class extension
                className = packageName + '.' + fileName.substring(0, fileName.length() - 6);
            }
            if (className != null) {
                processClass(className);
            }
            final File subDir = new File(dir, fileName);
            if (subDir.isDirectory()) {
                processDirectory(subDir, packageName + (!packageName.isEmpty() ? JAVA_PACKAGE_SEPARATOR : "") + fileName);
            } else if (isJar(subDir.getAbsolutePath())) {
                processJarFile(createJarFile(subDir.toURI().toURL()));
            }
        }
    }
}

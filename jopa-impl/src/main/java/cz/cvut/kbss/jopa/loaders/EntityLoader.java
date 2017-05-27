/**
 * Copyright (C) 2016 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.loaders;

import cz.cvut.kbss.jopa.model.JOPAPersistenceProperties;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.utils.Configuration;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.net.*;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.Objects;
import java.util.Set;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;

public class EntityLoader {

    private static final Logger LOG = LoggerFactory.getLogger(EntityLoader.class);

    private static final String JAR_FILE_SUFFIX = ".jar";
    private static final String CLASS_FILE_SUFFIX = ".class";

    /**
     * Discovers and returns all entity classes within the scan package and its subpackages.
     * <p>
     * I.e., it looks for classes annotated with {@link OWLClass}.
     *
     * @param configuration Persistence configuration, should contain value for the {@link
     *                      JOPAPersistenceProperties#SCAN_PACKAGE} property
     * @return Set of entity classes
     * @throws IllegalArgumentException If {@link JOPAPersistenceProperties#SCAN_PACKAGE} values is missing
     */
    public Set<Class<?>> discoverEntityClasses(Configuration configuration) {
        Objects.requireNonNull(configuration);
        if (!configuration.contains(JOPAPersistenceProperties.SCAN_PACKAGE)) {
            throw new IllegalArgumentException(
                    "Missing the " + JOPAPersistenceProperties.SCAN_PACKAGE + " property.");
        }
        String toScan = configuration.get(JOPAPersistenceProperties.SCAN_PACKAGE);
        if (toScan.isEmpty()) {
            throw new IllegalArgumentException(JOPAPersistenceProperties.SCAN_PACKAGE + " property cannot be empty.");
        }
        return discoverEntities(toScan);
    }


    /**
     * Inspired by https://github.com/ddopson/java-class-enumerator
     */
    private Set<Class<?>> discoverEntities(String scanPath) {
        final Set<Class<?>> all = new HashSet<>();
        final ClassLoader loader = Thread.currentThread().getContextClassLoader();
        try {
            Enumeration<URL> urls = loader.getResources(scanPath.replace('.', '/'));
            while (urls.hasMoreElements()) {
                final URL url = urls.nextElement();
                if (isJar(url.toString())) {
                    processJarFile(url, scanPath, all);
                } else {
                    processDirectory(new File(getUrlAsUri(url).getPath()), scanPath, all);
                }
            }
            // Scan jar files on classpath
            URL[] urlsSet = ((URLClassLoader) loader).getURLs();
            for (URL url : urlsSet) {
                if (isJar(url.toString())) {
                    processJarFile(url, scanPath, all);
                }
            }
        } catch (IOException e) {
            throw new JopaInitializationException("Unable to scan packages for entity classes.", e);
        }
        if (all.isEmpty()) {
            LOG.warn("No entity classes found in package " + scanPath);
        }
        return all;
    }

    private static boolean isJar(String filePath) {
        return filePath.startsWith("jar:") || filePath.endsWith(JAR_FILE_SUFFIX);
    }

    private static URI getUrlAsUri(URL url) {
        try {
            // Transformation to URI handles encoding, e.g. of whitespaces in the path
            return url.toURI();
        } catch (URISyntaxException ex) {
            throw new JopaInitializationException(
                    "Unable to scan resource " + url + ". It is not a valid URI.", ex);
        }
    }

    private void processJarFile(URL jarResource, String packageName, Set<Class<?>> entityClasses) {
        final String relPath = packageName.replace('.', '/');
        final String jarPath = jarResource.getPath().replaceFirst("[.]jar[!].*", JAR_FILE_SUFFIX)
                                          .replaceFirst("file:", "");

        LOG.trace("Scanning jar file {} for entity classes.", jarPath);
        try (final JarFile jarFile = new JarFile(jarPath)) {
            final Enumeration<JarEntry> entries = jarFile.entries();
            while (entries.hasMoreElements()) {
                final JarEntry entry = entries.nextElement();
                final String entryName = entry.getName();
                String className = null;
                if (entryName.endsWith(CLASS_FILE_SUFFIX) && entryName.startsWith(relPath)) {
                    className = entryName.replace('/', '.').replace('\\', '.');
                    className = className.substring(0, className.length() - CLASS_FILE_SUFFIX.length());
                }
                if (className != null) {
                    processClass(className, entityClasses);
                }
            }
        } catch (IOException e) {
            throw new JopaInitializationException("Unexpected IOException reading JAR File " + jarPath, e);
        }
    }

    private void processClass(String className, Set<Class<?>> entityClasses) {
        try {
            final Class<?> cls = Class.forName(className);
            if (cls.getAnnotation(OWLClass.class) != null) {
                entityClasses.add(cls);
            }
        } catch (ClassNotFoundException e) {
            throw new JopaInitializationException("Unexpected ClassNotFoundException when scanning for entities.", e);
        }
    }

    private void processDirectory(File dir, String packageName, Set<Class<?>> entityClasses)
            throws MalformedURLException {
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
                processClass(className, entityClasses);
            }
            final File subDir = new File(dir, fileName);
            if (subDir.isDirectory()) {
                processDirectory(subDir, packageName + (!packageName.isEmpty() ? '.' : "") + fileName, entityClasses);
            } else if (isJar(subDir.getAbsolutePath())) {
                processJarFile(subDir.toURI().toURL(), packageName, entityClasses);
            }
        }
    }
}

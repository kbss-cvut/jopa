package cz.cvut.kbss.jopa.loaders;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.*;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;
import java.util.logging.Level;
import java.util.logging.Logger;

import cz.cvut.kbss.jopa.model.annotations.OWLClass;

/**
 * Finds entity classes on the classpath.
 */
public class EntityLoader {

	private static final Logger LOG = Logger.getLogger(EntityLoader.class.getName());

	private static final String EXT = ".class";

	private Map<URL,ClassLoader> classUrls;
	private Set<Class<?>> classes;

	private EntityLoader() {
		this.classes = new HashSet<>();
	}

	/**
	 * Discovers and returns all entity classes, i. e. classes annotated with
	 * {@link OWLClass}.
	 * 
	 * @return Set of entity classes
	 */
	public static Set<Class<?>> discoverEntityClasses() {
		final long start = System.currentTimeMillis();
		final Set<Class<?>> classes = new EntityLoader().discoverEntities();
		final long end = System.currentTimeMillis();
		if (LOG.isLoggable(Level.FINE)) {
			LOG.fine("Discovering entities took " + (end - start) + "ms.");
		}
		return classes;
	}

	private Set<Class<?>> discoverEntities() {
		if (LOG.isLoggable(Level.FINE)) {
			LOG.fine("Searching for entity classes.");
		}
		loadUrls();
		for (URL url : classUrls.keySet()) {
			final File f = new File(url.getPath());
			if (!f.exists()) {
				continue;
			}
			discover(f, f, classUrls.get(url));
		}
		if (LOG.isLoggable(Level.CONFIG)) {
			// 5 is the statistically most probable length of a word + the comma
			// and space
			final StringBuilder sb = new StringBuilder(classes.size() * 7);
			for (Class<?> cls : classes) {
				sb.append(cls.getName()).append(", ");
			}
			LOG.config("The following entity classes were discovered: "
					+ sb.substring(0, sb.length() - 2));
		}
		return classes;
	}

	/**
	 * Recursively scans directories in order to discover Java class files.
	 * These files are then used to determine whether they contain entity
	 * classes.
	 */
	private void discover(File root, File file, ClassLoader c) {
		if (!file.exists()) {
			return;
		}
		final String fileName = file.getName();
		if (file.isDirectory()) {
			if (LOG.isLoggable(Level.FINEST)) {
				LOG.finest("Searching for entity classes in directory " + fileName);
			}
			for (File f : file.listFiles()) {
				if (!f.exists()) {
					continue;
				}
				discover(root, f, c);
			}
		} else if (fileName.toLowerCase().endsWith(".jar")) {
			if (LOG.isLoggable(Level.FINEST)) {
				LOG.finest("Searching for entity classes in jar file " + fileName);
			}
			JarFile jar = null;
			try {
				jar = new JarFile(file);
			} catch (IOException e) {
				// Do nothing
			}
			if (jar != null) {
				final Enumeration<JarEntry> entries = jar.entries();
				while (entries.hasMoreElements()) {
					JarEntry je = entries.nextElement();
					final String name = je.getName();
					final int extInd = name.lastIndexOf(EXT);
					if (extInd > 0) {
						final String clsName = name.substring(0, extInd).replace('/', '.');
						entity(clsName, c);
					}
				}
			}
		} else if (fileName.toLowerCase().endsWith(".class")) {
			entity(buildClassName(root, file),c);
		}
	}

	/**
	 * Gets default class loaders used to discover entity classes.
	 */
	private ClassLoader[] getClassLoaders() {
		final ClassLoader[] loaders = new ClassLoader[2];
		loaders[0] = Thread.currentThread().getContextClassLoader();
		loaders[1] = EntityLoader.class.getClassLoader();
		return loaders;
	}

	/**
	 * Finds the root URLs from which classes are loaded.
	 */
	private void loadUrls() {
		final Map<URL,ClassLoader> all = new HashMap<>();
		final ClassLoader[] loaders = getClassLoaders();
		try {
			for (ClassLoader c : loaders) {
				Enumeration<URL> urls = c.getResources("");
				while (urls.hasMoreElements()) {
					final URL url = urls.nextElement();
					final URL normalizedUrl = new URL(url.toExternalForm());
					all.put(normalizedUrl,c);
				}
			}
		} catch (IOException e) {
			throw new JopaInitializationException("Unable to load entity classes.", e);
		}
		if (LOG.isLoggable(Level.FINEST)) {
			LOG.finest("Found " + all.size() + " root URLs.");
		}
		this.classUrls = all;
	}

	/**
	 * Builds fully qualified class name. </p>
	 * 
	 * The class has the same name as {@code file} and fully qualified name
	 * starts in {@code root}.
	 */
	private String buildClassName(File root, File file) {
		final StringBuffer sb = new StringBuffer();
		final String fileName = file.getName();
		sb.append(fileName.substring(0, fileName.lastIndexOf(".class")));
		file = file.getParentFile();
		while (file != null && !file.equals(root)) {
			sb.insert(0, '.').insert(0, file.getName());
			file = file.getParentFile();
		}
		return sb.toString();
	}

	/**
	 * Determines whether class with the specified fully qualified name is an
	 * entity class.
	 */
	private void entity(String className ,ClassLoader c) {
		if (LOG.isLoggable(Level.FINER)) {
			LOG.finer("Testing class " + className + " for the OWLClass annotation.");
		}
		try {
			Class<?> cls = Class.forName(className,true,c);
			OWLClass a = cls.getAnnotation(OWLClass.class);
			if (a != null) {
				classes.add(cls);
			}
		} catch (ClassNotFoundException e) {
			// Do nothing
		}
	}
}

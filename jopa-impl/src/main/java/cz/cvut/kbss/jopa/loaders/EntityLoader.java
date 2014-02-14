package cz.cvut.kbss.jopa.loaders;

import java.io.IOException;
import java.net.URL;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.Set;

import org.reflections.Reflections;

import cz.cvut.kbss.jopa.model.annotations.OWLClass;

public class EntityLoader {

	private Set<URL> classUrls;

	private EntityLoader() {
	}

	/**
	 * Discovers and returns all entity classes, i. e. classes annotated with
	 * {@link OWLClass}.
	 * 
	 * @return Set of entity classes
	 */
	public static Set<Class<?>> discoverEntityClasses() {
		// TODO Add our custom implementation here
		// return new EntityLoader().discoverEntities();
		final Reflections r = new Reflections("");
		return r.getTypesAnnotatedWith(OWLClass.class);
	}

	private Set<Class<?>> discoverEntities() {
		loadUrls();
		// TODO
		return null;
	}

	private ClassLoader[] getClassLoaders() {
		final ClassLoader[] loaders = new ClassLoader[2];
		loaders[0] = Thread.currentThread().getContextClassLoader();
		loaders[1] = EntityLoader.class.getClassLoader();
		return loaders;
	}

	private void loadUrls() {
		final Set<URL> all = new HashSet<>();
		final ClassLoader[] loaders = getClassLoaders();
		try {
			for (ClassLoader c : loaders) {
				Enumeration<URL> urls = c.getResources("");
				while (urls.hasMoreElements()) {
					final URL url = urls.nextElement();
					final URL normalizedUrl = new URL(url.toExternalForm());
					all.add(normalizedUrl);
				}
			}
		} catch (IOException e) {
			throw new JopaInitializationException(
					"Unable to load entity classes.", e);
		}
		this.classUrls = all;
	}
}

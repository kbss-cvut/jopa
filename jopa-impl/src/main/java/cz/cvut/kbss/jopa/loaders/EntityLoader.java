package cz.cvut.kbss.jopa.loaders;

import java.io.IOException;
import java.net.URL;
import java.util.*;

import cz.cvut.kbss.jopa.owlapi.OWLAPIPersistenceProperties;
import org.reflections.Reflections;

import cz.cvut.kbss.jopa.model.annotations.OWLClass;

public class EntityLoader {

    private Set<URL> classUrls;

    private EntityLoader() {
    }

    /**
     * Discovers and returns all entity classes, i. e. classes annotated with {@link OWLClass}.
     *
     * @return Set of entity classes
     */
    public static Set<Class<?>> discoverEntityClasses(Map<String, String> properties) {
        // TODO Add our custom implementation here
        Objects.requireNonNull(properties);
        if (!properties.containsKey(OWLAPIPersistenceProperties.SCAN_PACKAGE)) {
            throw new IllegalArgumentException(
                    "Missing the " + OWLAPIPersistenceProperties.SCAN_PACKAGE + " property.");
        }
        String toScan = properties.get(OWLAPIPersistenceProperties.SCAN_PACKAGE);
        final Reflections r = new Reflections(toScan);
        return r.getTypesAnnotatedWith(OWLClass.class);
    }

    public static Set<Class<?>> discoverEntityClassesNew(Map<String, String> properties) {Objects.requireNonNull(properties);
        if (!properties.containsKey(OWLAPIPersistenceProperties.SCAN_PACKAGE)) {
            throw new IllegalArgumentException(
                    "Missing the " + OWLAPIPersistenceProperties.SCAN_PACKAGE + " property.");
        }
        String toScan = properties.get(OWLAPIPersistenceProperties.SCAN_PACKAGE);
        return new EntityLoader().discoverEntities(toScan);
    }

    private Set<Class<?>> discoverEntities(String scanPath) {
        loadUrls(scanPath);
        // TODO
        return null;
    }

    private ClassLoader[] getClassLoaders() {
        final ClassLoader[] loaders = new ClassLoader[2];
        loaders[0] = Thread.currentThread().getContextClassLoader();
        loaders[1] = EntityLoader.class.getClassLoader();
        return loaders;
    }

    private void loadUrls(String scanPath) {
        final Set<URL> all = new HashSet<>();
        final ClassLoader[] loaders = getClassLoaders();
        try {
            for (ClassLoader c : loaders) {
                Enumeration<URL> urls = c.getResources(scanPath.replace('.', '/'));
                while (urls.hasMoreElements()) {
                    final URL url = urls.nextElement();
                    final URL normalizedUrl = new URL(url.toExternalForm());
                    all.add(normalizedUrl);
                }
            }
        } catch (IOException e) {
            throw new JopaInitializationException(
                    "Unable to scan packages for entity classes.", e);
        }
        this.classUrls = all;
    }
}

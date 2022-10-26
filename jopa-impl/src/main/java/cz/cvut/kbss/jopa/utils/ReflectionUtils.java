package cz.cvut.kbss.jopa.utils;

import cz.cvut.kbss.jopa.exception.InstantiationException;

/**
 * Utility functions using Java Reflection API.
 */
public class ReflectionUtils {

    private ReflectionUtils() {
        throw new AssertionError();
    }

    /**
     * Creates a new instance of the specified class using the default no-arg constructor.
     * <p>
     * Note that it is expected that the no-arg constructor exists and is publicly accessible.
     *
     * @param cls Class to instantiate
     * @param <T> Type
     * @return New instance of class {@code cls}
     * @throws InstantiationException When no-arg constructor does not exist or is not accessible
     */
    public static  <T> T instantiateUsingDefaultConstructor(Class<T> cls) {
        // The main purpose of this method is to wrap object instantiation so that the deprecated Class.newInstance
        // calls can be replaced after migrating to newer Java version
        try {
            return cls.newInstance();
        } catch (java.lang.InstantiationException | IllegalAccessException e) {
            throw new InstantiationException(e);
        }
    }
}

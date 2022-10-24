package cz.cvut.kbss.jopa.environment.utils;

import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.loaders.DefaultClasspathScanner;

/**
 * Classpath scanner that skips classes annotated with {@link TestLocal}.
 */
public class TestClasspathScanner extends DefaultClasspathScanner {

    @Override
    protected void processClass(String className) {
        try {
            final Class<?> cls = Class.forName(className, true, classLoader);
            if (!cls.isAnnotationPresent(TestLocal.class)) {
                listeners.forEach(listener -> listener.accept(cls));
            }
        } catch (ClassNotFoundException e) {
            throw new OWLPersistenceException("Unexpected ClassNotFoundException when scanning for entities.", e);
        }
    }
}

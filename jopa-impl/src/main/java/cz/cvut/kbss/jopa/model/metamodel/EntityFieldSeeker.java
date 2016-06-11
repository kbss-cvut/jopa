package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.model.annotations.MappedSuperclass;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;

/**
 * Seeks fields in entity classes.
 */
public class EntityFieldSeeker {

    /**
     * Discovers all fields in the specified class and all its entity ancestor classes.
     *
     * @param entityClass The class to scan
     * @return Discovered fields
     */
    public Collection<Field> discoverFields(Class<?> entityClass) {
        final Collection<Field> result = new ArrayList<>();
        Class<?> current = entityClass;
        while (current.getSuperclass() != null && entityClass(current)) {
            result.addAll(Arrays.asList(current.getDeclaredFields()));
            current = current.getSuperclass();
        }
        return result;
    }

    private boolean entityClass(Class<?> cls) {
        return cls.getDeclaredAnnotation(OWLClass.class) != null
                || cls.getDeclaredAnnotation(MappedSuperclass.class) != null;
    }
}

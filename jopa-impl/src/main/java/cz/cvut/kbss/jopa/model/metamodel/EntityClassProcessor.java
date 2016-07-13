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
package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.exception.MetamodelInitializationException;
import cz.cvut.kbss.jopa.model.EntityTypeImpl;
import cz.cvut.kbss.jopa.model.IRI;
import cz.cvut.kbss.jopa.model.annotations.MappedSuperclass;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

/**
 * Utility methods for processing entities, related to metamodel construction.
 */
class EntityClassProcessor {

    private EntityClassProcessor() {
    }

    static <T> EntityTypeImpl<T> processEntityType(Class<T> cls) {
        final OWLClass c = cls.getAnnotation(OWLClass.class);

        if (c == null) {
            throw new MetamodelInitializationException("The class " + cls + " is not an OWLPersistence entity!");
        }

        checkForNoArgConstructor(cls);

        return new EntityTypeImpl<>(cls.getSimpleName(), cls, IRI.create(c.iri()));
    }

    private static <T> void checkForNoArgConstructor(Class<T> cls) {
        try {
            cls.getDeclaredConstructor();
        } catch (NoSuchMethodException e) {
            throw new MetamodelInitializationException("Class " + cls + " is missing required no-arg constructor.");
        }
    }

    /**
     * Resolves entity class hierarchy.
     * <p>
     * I.e. it traverses the {@code cls} and its ancestor classes until a non-entity class (neither an entity nor a
     * mapped superclass) is found (ultimately, this could be {@link Object}) and adds them into the result list.
     *
     * @param cls Class whose ancestor hierarchy should be returned
     * @return Entity hierarchy serialized into a sequence, starting with the most generic entity ancestor and ending
     * with {@code cls}
     */
    static <T> List<Class<? super T>> getEntityHierarchy(Class<T> cls) {
        final List<Class<? super T>> result = new ArrayList<>();
        Class<? super T> current = cls;
        while (current.getSuperclass() != null && entityClass(current)) {
            result.add(current);
            current = current.getSuperclass();
        }
        Collections.reverse(result);
        return result;
    }

    private static boolean entityClass(Class<?> cls) {
        return cls.getDeclaredAnnotation(OWLClass.class) != null
                || cls.getDeclaredAnnotation(MappedSuperclass.class) != null;
    }

    /**
     * Discovers all fields in the specified class and all its entity ancestor classes.
     *
     * @param entityClass The class to scan
     * @return Discovered fields
     */
    static <T> List<Field> getEntityFields(Class<T> entityClass) {
        final List<Field> result = new ArrayList<>();
        getEntityHierarchy(entityClass).forEach(cls -> result.addAll(Arrays.asList(cls.getDeclaredFields())));
        return result;
    }
}

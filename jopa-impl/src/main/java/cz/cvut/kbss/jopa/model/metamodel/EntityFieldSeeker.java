/**
 * Copyright (C) 2016 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
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

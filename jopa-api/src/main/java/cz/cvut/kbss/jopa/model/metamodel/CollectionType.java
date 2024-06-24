/*
 * JOPA
 * Copyright (C) 2024 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.model.metamodel;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;

public enum CollectionType {
    SET(Set.class), LIST(List.class), COLLECTION(Collection.class), MAP(Map.class);

    private final Class<?> collectionClass;

    CollectionType(Class<?> cls) {
        this.collectionClass = cls;
    }

    public static CollectionType fromClass(Class<?> cls) {
        for (CollectionType type : values()) {
            if (type.collectionClass.isAssignableFrom(cls)) {
                return type;
            }
        }
        throw new IllegalArgumentException("Unsupported collection type " + cls);
    }
}

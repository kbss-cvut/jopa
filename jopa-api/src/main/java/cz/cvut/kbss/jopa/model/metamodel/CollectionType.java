/*
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
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

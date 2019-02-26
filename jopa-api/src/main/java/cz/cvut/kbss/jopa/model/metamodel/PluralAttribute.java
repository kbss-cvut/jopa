/**
 * Copyright (C) 2019 Czech Technical University in Prague
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

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Instances of the type PluralAttribute represent persistent collection-valued
 * attributes.
 *
 * @param <X>
 *            The type the represented collection belongs to
 * @param <C>
 *            The type of the represented collection
 * @param <E>
 *            The element type of the represented collection
 */
public interface PluralAttribute<X, C, E> extends Attribute<X, C>, Bindable<E> {

    enum CollectionType {
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
            throw new IllegalArgumentException("Unsupported collection class " + cls);
        }
    }

    /**
     * Return the collection type.
     *
     * @return collection type
     */
    CollectionType getCollectionType();

    /**
     * Return the type representing the element type of the collection.
     *
     * @return element type
     */
    Type<E> getElementType();
}

/*
 * JOPA
 * Copyright (C) 2023 Czech Technical University in Prague
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

/**
 * Instances of the type PluralAttribute represent persistent collection-valued
 * attributes defined by a query.
 *
 * @param <X>
 *            The type the represented collection belongs to
 * @param <C>
 *            The type of the represented collection
 * @param <E>
 *            The element type of the represented collection
 */
public interface PluralQueryAttribute<X, C, E> extends QueryAttribute<X, C>, Bindable<E> {

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

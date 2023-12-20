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

import java.util.Set;

/**
 * Instances of the type SetAttribute represent persistent java.util.Set-valued
 * attributes.
 *
 * @param <X> The type the represented Set belongs to
 * @param <E> The element type of the represented Set
 */
public interface TypesSpecification<X, E> extends FieldSpecification<X, Set> {

    /**
     * Return the type representing the element type of the types collection.
     *
     * @return element type
     */
    Class<E> getElementType();
}

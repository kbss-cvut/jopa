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
package cz.cvut.kbss.jopa.model.query.criteria;

import cz.cvut.kbss.jopa.model.metamodel.SingularAttribute;

public interface Path<X> extends Expression<X> {

    /**
     * Create a path corresponding to the referenced attribute.
     *
     * @param attributeName name of the attribute
     * @return path corresponding to the referenced attribute
     * @throws IllegalArgumentException if attribute of the given name does not otherwise exist
     */
    <Y> Path<Y> getAttr(String attributeName);

    /**
     * Create a path corresponding to the referenced single-valued attribute.
     *
     * @param attribute single-valued attribute
     * @return path corresponding to the referenced attribute
     */
    <Y> Path<Y> getAttr(SingularAttribute<? super X, Y> attribute);

    /**
     * Return the parent "node" in the path or null if no parent.
     *
     * @return parent
     */
    Path<?> getParentPath();
}

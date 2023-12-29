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

import cz.cvut.kbss.jopa.UnusedJPA;

/**
 * Instances of the type SingularAttribute represents persistent single-valued
 * properties or fields.
 *
 * @param <X>
 *            The type containing the represented attribute
 * @param <T>
 *            The type of the represented attribute
 */
public interface SingularAttribute<X, T> extends Attribute<X, T>, Bindable<T> {

    /**
     * Is the attribute an id attribute. This method will return true if the
     * attribute is an attribute that corresponds to a simple id, an embedded
     * id, or an attribute of an id class.
     *
     * @return boolean indicating whether the attribute is an id
     */
    boolean isId();

    /**
     * Is the attribute a version attribute.
     *
     * @return boolean indicating whether the attribute is a version attribute
     * @deprecated Not supported in JOPA
     */
    @UnusedJPA
    @Deprecated
    boolean isVersion();

    /**
     * Return the type that represents the type of the attribute.
     *
     * @return type of attribute
     */
    Type<T> getType();
}

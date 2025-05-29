/*
 * JOPA
 * Copyright (C) 2025 Czech Technical University in Prague
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
 * Instances of the type SingularAttribute represents persistent single-valued properties or fields.
 *
 * @param <X> The type containing the represented attribute
 * @param <T> The type of the represented attribute
 */
public interface SingularAttribute<X, T> extends Attribute<X, T>, Bindable<T> {

    /**
     * Is the attribute an id attribute.
     * <p>
     * Always returns false, as identifiers are represented by {@link Identifier} attributes.
     *
     * @return {@code false}
     */
    default boolean isId() {
        return false;
    }

    /**
     * Is the attribute a version attribute.
     *
     * @return boolean indicating whether the attribute is a version attribute
     */
    @UnusedJPA
    boolean isVersion();

    /**
     * Return the type that represents the type of the attribute.
     *
     * @return type of attribute
     */
    Type<T> getType();
}

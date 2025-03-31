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

/**
 * Instances of the type Bindable represent object or attribute types that can
 * be bound into a Path.
 *
 * @param <T>
 *            The type of the represented object or attribute
 */
public interface Bindable<T> {
    enum BindableType {
        SINGULAR_ATTRIBUTE, PLURAL_ATTRIBUTE, ENTITY_TYPE
    }

    /**
     * Return the bindable type of the represented object.
     *
     * @return bindable type
     */
    BindableType getBindableType();

    /**
     * Return the Java type of the represented object. If the bindable type of
     * the object is {@link BindableType#PLURAL_ATTRIBUTE}, the Java element type is returned. If the
     * bindable type is {@link BindableType#SINGULAR_ATTRIBUTE} or {@link BindableType#ENTITY_TYPE}, the Java type of the
     * represented entity or attribute is returned.
     *
     * @return Java type
     */
    Class<T> getBindableJavaType();
}

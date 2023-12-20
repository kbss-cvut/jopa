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
package cz.cvut.kbss.ontodriver.descriptor;

import cz.cvut.kbss.ontodriver.model.NamedResource;

import java.util.List;

/**
 * Descriptor of list values.
 *
 * @param <T> Value type
 */
public interface ListValueDescriptor<T> extends ListDescriptor {

    /**
     * Gets values from the list described by this descriptor.
     *
     * @return List of value identifiers
     */
    List<T> getValues();

    /**
     * Adds value to this list descriptor.
     *
     * @param elem The value to add, i.e. identifier of the list element
     */
    void addValue(T elem);
}

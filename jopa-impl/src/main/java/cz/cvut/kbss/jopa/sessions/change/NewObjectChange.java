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
package cz.cvut.kbss.jopa.sessions.change;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;

import java.util.Objects;

/**
 * Represents addition of a new object.
 */
public class NewObjectChange implements Change {

    private final Object object;

    private final Descriptor descriptor;

    public NewObjectChange(Object object, Descriptor descriptor) {
        this.object = Objects.requireNonNull(object);
        this.descriptor = Objects.requireNonNull(descriptor);
    }

    @Override
    public Object getClone() {
        return object;
    }

    @Override
    public Object getOriginal() {
        return null;
    }

    @Override
    public Descriptor getDescriptor() {
        return descriptor;
    }
}

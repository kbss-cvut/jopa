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
package cz.cvut.kbss.jopa.sessions.change;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;

import java.util.Objects;

/**
 * Represents object deletion.
 */
public class DeleteObjectChange implements Change {

    private final Object clone;

    private final Object original;

    private final Descriptor descriptor;

    public DeleteObjectChange(Object clone, Object original, Descriptor descriptor) {
        this.clone = Objects.requireNonNull(clone);
        this.original = Objects.requireNonNull(original);
        this.descriptor = Objects.requireNonNull(descriptor);
    }


    @Override
    public Object getClone() {
        return clone;
    }

    @Override
    public Object getOriginal() {
        return original;
    }

    @Override
    public Descriptor getDescriptor() {
        return descriptor;
    }
}

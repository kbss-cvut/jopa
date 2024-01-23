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
package cz.cvut.kbss.jopa.api;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.function.Consumer;

public class CloneConfiguration {

    private final Descriptor descriptor;

    private final List<Consumer<Object>> postRegister = new ArrayList<>(1);

    private final boolean forPersistenceContext;

    public CloneConfiguration(Descriptor descriptor, boolean forPersistenceContext) {
        this.descriptor = Objects.requireNonNull(descriptor);
        this.forPersistenceContext = forPersistenceContext;
    }

    public CloneConfiguration(Descriptor descriptor, boolean forPersistenceContext, List<Consumer<Object>> handlers) {
        this.descriptor = Objects.requireNonNull(descriptor);
        this.forPersistenceContext = forPersistenceContext;
        Objects.requireNonNull(handlers);
        postRegister.addAll(handlers);
    }

    public void addPostRegisterHandler(Consumer<Object> handler) {
        postRegister.add(handler);
    }

    public Descriptor getDescriptor() {
        return descriptor;
    }

    public List<Consumer<Object>> getPostRegister() {
        return Collections.unmodifiableList(postRegister);
    }

    public boolean isForPersistenceContext() {
        return forPersistenceContext;
    }
}

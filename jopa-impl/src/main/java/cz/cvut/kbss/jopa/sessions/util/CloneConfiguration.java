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
package cz.cvut.kbss.jopa.sessions.util;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.function.Consumer;

public class CloneConfiguration {

    private Descriptor descriptor;

    private final List<Consumer<Object>> postRegister = new ArrayList<>(1);

    private boolean forPersistenceContext;

    private CloneConfiguration() {
    }

    public CloneConfiguration(Descriptor descriptor, boolean forPersistenceContext) {
        this.descriptor = Objects.requireNonNull(descriptor);
        this.forPersistenceContext = forPersistenceContext;
    }

    public CloneConfiguration addPostRegisterHandlers(Collection<Consumer<Object>> handlers) {
        postRegister.addAll(handlers);
        return this;
    }

    public Descriptor getDescriptor() {
        return descriptor;
    }

    public List<Consumer<Object>> getPostRegister() {
        return Collections.unmodifiableList(postRegister);
    }

    public CloneConfiguration forPersistenceContext(boolean value) {
        this.forPersistenceContext = value;
        return this;
    }

    public boolean isForPersistenceContext() {
        return forPersistenceContext;
    }

    public static CloneConfiguration withDescriptor(Descriptor descriptor) {
        final CloneConfiguration configuration = new CloneConfiguration();
        configuration.descriptor = descriptor;
        return configuration;
    }
}

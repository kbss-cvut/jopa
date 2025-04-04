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
import java.util.List;
import java.util.Objects;
import java.util.function.Consumer;

public class CloneRegistrationDescriptor {

    private final Descriptor descriptor;

    private final List<Consumer<Object>> postCloneHandlers = new ArrayList<>(1);

    private boolean allEager;

    public CloneRegistrationDescriptor(Descriptor descriptor) {
        this.descriptor = descriptor;
    }

    public Descriptor getDescriptor() {
        return descriptor;
    }

    public CloneRegistrationDescriptor allEager(boolean value) {
        this.allEager = value;
        return this;
    }

    public boolean isAllEager() {
        return allEager;
    }

    public CloneRegistrationDescriptor postCloneHandlers(Collection<Consumer<Object>> handlers) {
        postCloneHandlers.addAll(handlers);
        return this;
    }

    public List<Consumer<Object>> getPostCloneHandlers() {
        return postCloneHandlers;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof CloneRegistrationDescriptor that)) {
            return false;
        }
        return isAllEager() == that.isAllEager() && Objects.equals(getDescriptor(),
                that.getDescriptor()) && Objects.equals(
                getPostCloneHandlers(), that.getPostCloneHandlers());
    }

    @Override
    public int hashCode() {
        return Objects.hash(getDescriptor(), getPostCloneHandlers(), isAllEager());
    }
}

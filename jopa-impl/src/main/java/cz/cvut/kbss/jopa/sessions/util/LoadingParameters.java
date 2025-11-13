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

import java.net.URI;

public record LoadingParameters<T> (Class<T> entityClass, URI identifier, Descriptor descriptor, boolean forceEager, boolean bypassCache) {

    public LoadingParameters(Class<T> cls, URI identifier, Descriptor descriptor) {
        this(cls, identifier, descriptor, false, false);
    }

    public LoadingParameters(Class<T> cls, URI identifier, Descriptor descriptor, boolean forceEager) {
        this(cls, identifier, descriptor, forceEager, false);
    }

    public LoadingParameters<T> withBypassCache() {
        return new LoadingParameters<>(entityClass, identifier, descriptor, forceEager, true);
    }
}

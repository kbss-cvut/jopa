/*
 * JOPA
 * Copyright (C) 2026 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.model;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;

import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Factory that caches created descriptors.
 * <p>
 * This assumes the descriptors are not modified by the caller.
 */
public class CachingEntityDescriptorFactory implements EntityDescriptorFactory {

    private final EntityDescriptorFactory factory;

    private final Map<Class<?>, Descriptor> cache = new ConcurrentHashMap<>();

    public CachingEntityDescriptorFactory(EntityDescriptorFactory factory) {
        this.factory = factory;
    }

    @Override
    public <T> Descriptor createDescriptor(Class<T> cls) {
        Objects.requireNonNull(cls);
        return cache.computeIfAbsent(cls, factory::createDescriptor);
    }
}

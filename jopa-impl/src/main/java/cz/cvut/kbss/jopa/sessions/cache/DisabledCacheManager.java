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
package cz.cvut.kbss.jopa.sessions.cache;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.sessions.descriptor.LoadStateDescriptor;

import java.net.URI;
import java.util.Set;

public class DisabledCacheManager implements CacheManager {

    @Override
    public void add(Object identifier, Object entity, Descriptors descriptors) {
        // Do nothing
    }

    @Override
    public <T> T get(Class<T> cls, Object identifier, Descriptor descriptor) {
        return null;
    }


    @Override
    public LoadStateDescriptor<?> getLoadStateDescriptor(Object instance) {
        return null;
    }

    @Override
    public void evictInferredObjects() {
        // Do nothing
    }

    @Override
    public void setInferredClasses(Set<Class<?>> inferredClasses) {
        // Do nothing
    }

    @Override
    public void close() {
        // Do nothing
    }

    @Override
    public boolean contains(Class<?> cls, Object identifier, Descriptor descriptor) {
        return false;
    }

    @Override
    public void evict(Class<?> cls, Object identifier, URI context) {
        // Do nothing
    }

    @Override
    public void evict(Class<?> cls) {
        // Do nothing
    }

    @Override
    public void evict(URI contextUri) {
        // Do nothing
    }

    @Override
    public void evictAll() {
        // Do nothing
    }
}

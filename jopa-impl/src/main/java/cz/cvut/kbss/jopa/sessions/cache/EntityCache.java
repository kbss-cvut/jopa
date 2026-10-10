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
package cz.cvut.kbss.jopa.sessions.cache;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.sessions.descriptor.LoadStateDescriptor;
import cz.cvut.kbss.jopa.utils.MetamodelUtils;

import java.net.URI;
import java.util.Collections;
import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.Map;
import java.util.Set;
import java.util.function.Consumer;

class EntityCache {

    // TODO Think about locking on context level, so that the whole cache doesn't have to be locked when being accessed

    private static final String DEFAULT_CONTEXT_BASE = "http://defaultContext";

    final Map<URI, Map<Object, Map<Class<?>, Object>>> repoCache;
    final Map<Object, Descriptors> descriptors;
    final URI defaultContext;

    EntityCache() {
        repoCache = new HashMap<>();
        this.descriptors =
                new IdentityHashMap<>(); // Need to use identity to cope with entities overriding equals/hashcode
        this.defaultContext = URI.create(DEFAULT_CONTEXT_BASE + System.currentTimeMillis());
    }

    void put(Object identifier, Object entity, Descriptors descriptors) {
        assert identifier != null;
        assert entity != null;
        assert isCacheable(descriptors.repositoryDescriptor());

        final Class<?> cls = entityClass(entity);
        final URI ctx = descriptors.repositoryDescriptor().getSingleContext().orElse(defaultContext);

        final Map<Object, Map<Class<?>, Object>> ctxMap = repoCache.computeIfAbsent(ctx, k -> new HashMap<>());
        final Map<Class<?>, Object> individualMap = ctxMap.computeIfAbsent(identifier, k -> new HashMap<>());
        final Object previous = individualMap.put(cls, entity);
        if (previous != null) {
            this.descriptors.remove(previous);
        }
        this.descriptors.put(entity, descriptors);
    }

    boolean isCacheable(Descriptor descriptor) {
        return descriptor.getContexts().size() <= 1;
    }

    Class<?> entityClass(Object entity) {
        return MetamodelUtils.getEntityClass(entity.getClass());
    }

    <T> T get(Class<T> cls, Object identifier, Descriptor descriptor) {
        return getInternal(cls, identifier, descriptor, u -> {});
    }

    <T> T getInternal(Class<T> cls, Object identifier, Descriptor descriptor, Consumer<URI> contextHandler) {
        assert cls != null;
        assert identifier != null;

        final Set<URI> contexts =
                descriptor.getContexts().isEmpty() ? Collections.singleton(defaultContext) : descriptor.getContexts();
        for (URI ctx : contexts) {
            final Map<Class<?>, Object> m = getMapForId(ctx, identifier);
            final Object result = m.get(cls);
            if (result != null && descriptors.get(result).repositoryDescriptor().equals(descriptor)) {
                contextHandler.accept(ctx);
                return cls.cast(result);
            }
        }
        return null;
    }

    <T> LoadStateDescriptor<T> getLoadStateDescriptor(T instance) {
        final Descriptors d = descriptors.get(instance);
        return d != null ? (LoadStateDescriptor<T>) d.loadStateDescriptor() : null;
    }

    boolean contains(Class<?> cls, Object identifier, Descriptor descriptor) {
        assert cls != null;
        assert identifier != null;
        assert descriptor != null;

        final Set<URI> contexts =
                descriptor.getContexts().isEmpty() ? Collections.singleton(defaultContext) : descriptor.getContexts();
        for (URI ctx : contexts) {
            final Map<Class<?>, Object> m = getMapForId(ctx, identifier);
            final Object result = m.get(cls);
            if (result == null) {
                continue;
            }
            assert descriptors.containsKey(result);

            if (descriptors.get(result).repositoryDescriptor().equals(descriptor)) {
                return true;
            }
        }
        return false;
    }

    void evict(Class<?> cls, Object identifier, URI context) {
        assert cls != null;
        assert identifier != null;

        final Map<Class<?>, Object> m = getMapForId(context, identifier);
        final Object removed = m.remove(cls);
        if (removed != null) {
            descriptors.remove(removed);
        }
    }

    void evict(URI context) {
        if (context == null) {
            context = defaultContext;
        }
        final Map<Object, Map<Class<?>, Object>> contextCache = repoCache.remove(context);
        if (contextCache == null) {
            return;
        }
        contextCache.values().forEach(instances -> instances.values().forEach(descriptors::remove));
    }

    void evict(Class<?> cls) {
        for (Map.Entry<URI, Map<Object, Map<Class<?>, Object>>> e : repoCache.entrySet()) {
            final Map<Object, Map<Class<?>, Object>> m = e.getValue();
            m.forEach((key, value) -> {
                final Object removed = value.remove(cls);
                if (removed != null) {
                    descriptors.remove(removed);
                }
            });
        }
    }

    private Map<Class<?>, Object> getMapForId(URI context, Object identifier) {
        assert identifier != null;

        final URI ctx = context != null ? context : defaultContext;

        final Map<Object, Map<Class<?>, Object>> ctxMap = repoCache.get(ctx);
        return ctxMap != null ? ctxMap.getOrDefault(identifier, Collections.emptyMap()) : Collections.emptyMap();
    }
}

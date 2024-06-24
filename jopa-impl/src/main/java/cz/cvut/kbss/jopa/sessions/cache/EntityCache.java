/*
 * JOPA
 * Copyright (C) 2024 Czech Technical University in Prague
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

        final Class<?> cls = entity.getClass();
        final URI ctx = descriptors.repositoryDescriptor().getSingleContext().orElse(defaultContext);

        Map<Object, Map<Class<?>, Object>> ctxMap;
        if (!repoCache.containsKey(ctx)) {
            ctxMap = new HashMap<>();
            repoCache.put(ctx, ctxMap);
        } else {
            ctxMap = repoCache.get(ctx);
        }
        Map<Class<?>, Object> individualMap;
        if (!ctxMap.containsKey(identifier)) {
            individualMap = new HashMap<>();
            ctxMap.put(identifier, individualMap);
        } else {
            individualMap = ctxMap.get(identifier);
        }
        if (individualMap.containsKey(cls)) {
            this.descriptors.remove(individualMap.get(cls));
        }
        individualMap.put(cls, entity);
        this.descriptors.put(entity, descriptors);
    }

    boolean isCacheable(Descriptor descriptor) {
        return descriptor.getContexts().size() <= 1;
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
        return descriptors.containsKey(instance) ?
               (LoadStateDescriptor<T>) descriptors.get(instance).loadStateDescriptor() : null;
    }

    boolean contains(Class<?> cls, Object identifier, Descriptor descriptor) {
        assert cls != null;
        assert identifier != null;
        assert descriptor != null;

        final Set<URI> contexts =
                descriptor.getContexts().isEmpty() ? Collections.singleton(defaultContext) : descriptor.getContexts();
        for (URI ctx : contexts) {
            final Map<Class<?>, Object> m = getMapForId(ctx, identifier);
            if (!m.containsKey(cls)) {
                continue;
            }
            final Object result = m.get(cls);
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
        if (m.containsKey(cls)) {
            descriptors.remove(m.get(cls));
        }
        m.remove(cls);
    }

    void evict(URI context) {
        if (context == null) {
            context = defaultContext;
        }
        if (!repoCache.containsKey(context)) {
            return;
        }
        final Map<Object, Map<Class<?>, Object>> contextCache = repoCache.remove(context);
        contextCache.values().forEach(instances -> instances.values().forEach(descriptors::remove));
    }

    void evict(Class<?> cls) {
        for (Map.Entry<URI, Map<Object, Map<Class<?>, Object>>> e : repoCache.entrySet()) {
            final Map<Object, Map<Class<?>, Object>> m = e.getValue();
            m.forEach((key, value) -> {
                if (value.containsKey(cls)) {
                    descriptors.remove(value.get(cls));
                    value.remove(cls);
                }
            });
        }
    }

    private Map<Class<?>, Object> getMapForId(URI context, Object identifier) {
        assert identifier != null;

        final URI ctx = context != null ? context : defaultContext;

        if (!repoCache.containsKey(ctx)) {
            return Collections.emptyMap();
        }
        final Map<Object, Map<Class<?>, Object>> ctxMap = repoCache.get(ctx);
        return ctxMap.getOrDefault(identifier, Collections.emptyMap());
    }
}

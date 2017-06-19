/**
 * Copyright (C) 2016 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.sessions.cache;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;

import java.net.URI;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

class EntityCache {

    // TODO Think about locking on context level, so that the whole cache doesn't have to be locked when being accessed

    private static final String DEFAULT_CONTEXT_BASE = "http://defaultContext";

    final Map<URI, Map<Object, Map<Class<?>, Object>>> repoCache;
    final Map<Object, Descriptor> descriptors;
    final URI defaultContext;

    EntityCache() {
        repoCache = new HashMap<>();
        this.descriptors = new HashMap<>();
        this.defaultContext = URI.create(DEFAULT_CONTEXT_BASE + System.currentTimeMillis());
    }

    void put(Object identifier, Object entity, Descriptor descriptor) {
        assert identifier != null;
        assert entity != null;

        final Class<?> cls = entity.getClass();
        final URI ctx = descriptor.getContext() != null ? descriptor.getContext() : defaultContext;

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
        individualMap.put(cls, entity);
        descriptors.put(entity, descriptor);
    }

    <T> T get(Class<T> cls, Object identifier, Descriptor descriptor) {
        assert cls != null;
        assert identifier != null;

        final URI ctx = descriptor.getContext() != null ? descriptor.getContext() : defaultContext;
        final Map<Class<?>, Object> m = getMapForId(ctx, identifier);
        return cls.cast(m.getOrDefault(cls, null));
    }

    boolean contains(Class<?> cls, Object identifier, Descriptor descriptor) {
        assert cls != null;
        assert identifier != null;
        assert descriptor != null;

        final Map<Class<?>, Object> m = getMapForId(descriptor.getContext(), identifier);
        return m.containsKey(cls);
    }

    void evict(Class<?> cls, Object identifier, URI context) {
        assert cls != null;
        assert identifier != null;

        final URI ctx = context != null ? context : defaultContext;
        final Map<Class<?>, Object> m = getMapForId(ctx, identifier);
        m.remove(cls);
    }

    void evict(URI context) {
        if (context == null) {
            context = defaultContext;
        }
        if (!repoCache.containsKey(context)) {
            return;
        }
        repoCache.get(context).clear();
    }

    void evict(Class<?> cls) {
        for (Map.Entry<URI, Map<Object, Map<Class<?>, Object>>> e : repoCache.entrySet()) {
            final Map<Object, Map<Class<?>, Object>> m = e.getValue();
            m.forEach((key, value) -> value.remove(cls));
        }
    }

    private Map<Class<?>, Object> getMapForId(URI context, Object identifier) {
        assert context != null;
        assert identifier != null;

        if (!repoCache.containsKey(context)) {
            return Collections.emptyMap();
        }
        final Map<Object, Map<Class<?>, Object>> ctxMap = repoCache.get(context);
        return ctxMap.getOrDefault(identifier, Collections.emptyMap());
    }
}

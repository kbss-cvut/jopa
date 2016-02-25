/**
 * Copyright (C) 2016 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.sessions.cache;

import java.net.URI;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

/**
 * TODO Think about locking on context level, so that the whole cache doesn't have to be locked when being accessed
 * @author ledvima1
 */
class EntityCache {

    private static final String DEFAULT_CONTEXT_BASE = "http://defaultContext";

    final Map<URI, Map<Class<?>, Map<Object, Object>>> repoCache;
    final URI defaultContext;

    EntityCache() {
        repoCache = new HashMap<>();
        this.defaultContext = URI.create(DEFAULT_CONTEXT_BASE + System.currentTimeMillis());
    }

    void put(Object primaryKey, Object entity, URI context) {
        assert primaryKey != null;
        assert entity != null;

        final Class<?> cls = entity.getClass();
        final URI ctx = context != null ? context : defaultContext;

        Map<Class<?>, Map<Object, Object>> ctxMap;
        if (!repoCache.containsKey(ctx)) {
            ctxMap = new HashMap<>();
            repoCache.put(ctx, ctxMap);
        } else {
            ctxMap = repoCache.get(ctx);
        }
        Map<Object, Object> clsMap;
        if (!ctxMap.containsKey(cls)) {
            clsMap = new HashMap<>();
            ctxMap.put(cls, clsMap);
        } else {
            clsMap = ctxMap.get(cls);
        }
        clsMap.put(primaryKey, entity);
    }

    <T> T get(Class<T> cls, Object primaryKey, URI context) {
        assert cls != null;
        assert primaryKey != null;

        final URI ctx = context != null ? context : defaultContext;
        final Map<Object, Object> m = getMapForClass(ctx, cls);
        if (m.containsKey(primaryKey)) {
            return cls.cast(m.get(primaryKey));
        }
        return null;
    }

    boolean contains(Class<?> cls, Object primaryKey) {
        assert cls != null;
        assert primaryKey != null;
        final Map<Object, Object> m = getMapForClass(defaultContext, cls);
        return m.containsKey(primaryKey);
    }

    boolean contains(Class<?> cls, Object primaryKey, URI context) {
        assert cls != null;
        assert primaryKey != null;
        if (context == null) {
            return contains(cls, primaryKey);
        }

        final Map<Object, Object> m = getMapForClass(context, cls);
        return m.containsKey(primaryKey);
    }

    void evict(Class<?> cls, Object primaryKey, URI context) {
        assert cls != null;
        assert primaryKey != null;

        final URI ctx = context != null ? context : defaultContext;
        final Map<Object, Object> m = getMapForClass(ctx, cls);
        if (m.containsKey(primaryKey)) {
            m.remove(primaryKey);
        }
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
        for (Map.Entry<URI, Map<Class<?>, Map<Object, Object>>> e : repoCache.entrySet()) {
            final Map<Class<?>, Map<Object, Object>> m = e.getValue();
            m.remove(cls);
        }
    }

    private Map<Object, Object> getMapForClass(URI context, Class<?> cls) {
        assert context != null;
        assert cls != null;

        if (!repoCache.containsKey(context)) {
            return Collections.emptyMap();
        }
        final Map<Class<?>, Map<Object, Object>> ctxMap = repoCache.get(context);
        return (ctxMap.containsKey(cls) ? ctxMap.get(cls) : Collections.emptyMap());
    }
}

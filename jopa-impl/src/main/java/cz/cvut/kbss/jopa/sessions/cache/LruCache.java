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

import java.net.URI;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.function.Consumer;

class LruCache extends LinkedHashMap<LruCache.CacheNode, Object> {

    private final int capacity;
    private final transient Consumer<CacheNode> removeCallback;

    LruCache(int initialCapacity, Consumer<CacheNode> removeCallback) {
        super(initialCapacity, 1.0f, true);
        this.capacity = initialCapacity;
        this.removeCallback = removeCallback;
    }

    @Override
    protected boolean removeEldestEntry(Map.Entry<CacheNode, Object> eldest) {
        if (this.size() >= capacity) {
            removeCallback.accept(eldest.getKey());
            return true;
        }
        return false;
    }


    public static class CacheNode {
        private final URI context;
        private final Class<?> cls;
        private final Object identifier;

        CacheNode(URI context, Class<?> cls, Object identifier) {
            assert context != null;
            assert cls != null;
            assert identifier != null;

            this.context = context;
            this.cls = cls;
            this.identifier = identifier;
        }

        public URI getContext() {
            return context;
        }

        public Class<?> getCls() {
            return cls;
        }

        public Object getIdentifier() {
            return identifier;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;

            CacheNode cacheNode = (CacheNode) o;

            return context.equals(cacheNode.context) && cls.equals(cacheNode.cls) &&
                    identifier.equals(cacheNode.identifier);

        }

        @Override
        public int hashCode() {
            int result = context.hashCode();
            result = 31 * result + cls.hashCode();
            result = 31 * result + identifier.hashCode();
            return result;
        }
    }
}

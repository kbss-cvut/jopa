package cz.cvut.kbss.jopa.sessions.cache;

import java.net.URI;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.function.Consumer;

/**
 * @author ledvima1
 */
class LruCache extends LinkedHashMap<LruCache.CacheNode, Object> {

    private final int capacity;
    private final Consumer<CacheNode> removeCallback;

    public LruCache(int initialCapacity, Consumer<CacheNode> removeCallback) {
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

        public CacheNode(URI context, Class<?> cls, Object identifier) {
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

package cz.cvut.kbss.jopa.sessions.cache;

import cz.cvut.kbss.jopa.owlapi.OWLAPIPersistenceProperties;
import cz.cvut.kbss.jopa.sessions.CacheManager;

import java.util.Map;
import java.util.Objects;

/**
 * Creates second level cache based on the specified properties.
 *
 * @author ledvima1
 */
public abstract class CacheFactory {

    private static final String LRU_CACHE = "lru";
    private static final String TTL_CACHE = "ttl";

    private CacheFactory() {
        throw new AssertionError();
    }

    /**
     * Creates new cache based on the specified properties.
     *
     * @param properties Configuration of cache
     * @return Cache implementation
     */
    public static CacheManager createCache(Map<String, String> properties) {
        Objects.requireNonNull(properties);
        final String enabledStr = properties.get(OWLAPIPersistenceProperties.CACHE_ENABLED);
        if (enabledStr != null && !Boolean.parseBoolean(enabledStr)) {
            return new DisabledCacheManager();
        }
        return createEnabledCache(properties);
    }

    private static CacheManager createEnabledCache(Map<String, String> properties) {
        final String cacheType = properties.getOrDefault(OWLAPIPersistenceProperties.CACHE_TYPE, LRU_CACHE)
                                           .toLowerCase();
        switch (cacheType) {
            case LRU_CACHE:
                return new LruCacheManager(properties);
            case TTL_CACHE:
                return new TtlCacheManager(properties);
            default:
                throw new IllegalArgumentException("Invalid second level cache type " + cacheType);
        }
    }
}

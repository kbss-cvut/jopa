/**
 * Copyright (C) 2011 Czech Technical University in Prague
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

import cz.cvut.kbss.jopa.model.JOPAPersistenceProperties;
import cz.cvut.kbss.jopa.sessions.CacheManager;

import java.util.Map;
import java.util.Objects;
import java.util.logging.Logger;

/**
 * Creates second level cache based on the specified properties.
 *
 * @author ledvima1
 */
public abstract class CacheFactory {

    private static final Logger LOG = Logger.getLogger(CacheFactory.class.getName());

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
        final String enabledStr = properties.get(JOPAPersistenceProperties.CACHE_ENABLED);
        if (enabledStr != null && !Boolean.parseBoolean(enabledStr)) {
            LOG.config("Second level cache is disabled.");
            return new DisabledCacheManager();
        }
        return createEnabledCache(properties);
    }

    private static CacheManager createEnabledCache(Map<String, String> properties) {
        final String cacheType = properties.getOrDefault(JOPAPersistenceProperties.CACHE_TYPE, LRU_CACHE)
                                           .toLowerCase();
        switch (cacheType) {
            case LRU_CACHE:
                LOG.config("Using LRU cache.");
                return new LruCacheManager(properties);
            case TTL_CACHE:
                LOG.config("Using TTL cache.");
                return new TtlCacheManager(properties);
            default:
                throw new IllegalArgumentException("Invalid second level cache type " + cacheType);
        }
    }
}

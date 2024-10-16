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
package cz.cvut.kbss.jopa.model;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.ref.ReferenceQueue;
import java.lang.ref.SoftReference;
import java.lang.ref.WeakReference;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Optional;
import java.util.ServiceConfigurationError;
import java.util.ServiceLoader;

/**
 * Default implementation of the {@link PersistenceProviderResolver}, threadsafe.
 * <p>
 * Uses service loading mechanism ({@link ServiceLoader#load(Class, ClassLoader)} using the current context thread's class loader) to discover {@link PersistenceProvider} implementations on the classpath.
 * <p>
 * Code based on JPA specification implementation.
 */
public class DefaultPersistenceProviderResolver implements PersistenceProviderResolver {

    private static final Logger LOG = LoggerFactory.getLogger(DefaultPersistenceProviderResolver.class);

    /**
     * Queue for reference objects referring to class loaders or persistence providers.
     */
    private static final ReferenceQueue<?> referenceQueue = new ReferenceQueue<>();

    /**
     * Available providers cached by CacheKey to ensure there is no potential for provider visibility issues.
     */
    private final HashMap<CacheKey, PersistenceProviderReference> providers = new HashMap<>();

    public List<PersistenceProvider> getPersistenceProviders() {
        // Before we do the real loading work, see whether we need to
        // do some cleanup: If references to class loaders or
        // persistence providers have been nulled out, remove all related
        // information from the cache.
        processQueue();

        final ClassLoader loader = Thread.currentThread().getContextClassLoader();
        final CacheKey cacheKey = new CacheKey(loader);
        PersistenceProviderReference providersReferent = providers.get(cacheKey);

        if (providersReferent != null) {
            return providersReferent.get();
        }

        List<PersistenceProvider> loadedProviders = new ArrayList<>();
        Iterator<PersistenceProvider> ipp = ServiceLoader.load(PersistenceProvider.class, loader).iterator();
        try {
            loadProvider(ipp).ifPresent(loadedProviders::add);
        } catch (ServiceConfigurationError sce) {
            LOG.warn("Unable to load PersistenceProvider implementation via service loader.", sce);
        }

        if (loadedProviders.isEmpty()) {
            LOG.warn("No valid providers found.");
        }

        providersReferent = new PersistenceProviderReference(loadedProviders, referenceQueue, cacheKey);

        providers.put(cacheKey, providersReferent);

        return loadedProviders;
    }

    private static Optional<PersistenceProvider> loadProvider(Iterator<PersistenceProvider> ipp) {
        try {
            return Optional.of(ipp.next());
        } catch (ServiceConfigurationError sce) {
            LOG.warn("Unable to load PersistenceProvider implementation via service loader.", sce);
            return Optional.empty();
        }
    }

    /**
     * Remove garbage collected cache keys & providers.
     */
    private void processQueue() {
        CacheKeyReference ref;
        while ((ref = (CacheKeyReference) referenceQueue.poll()) != null) {
            providers.remove(ref.getCacheKey());
        }
    }

    /**
     * Clear all cached providers
     */
    public void clearCachedProviders() {
        providers.clear();
    }


    /**
     * The common interface to get a CacheKey implemented by
     * LoaderReference and PersistenceProviderReference.
     */
    private interface CacheKeyReference {
        CacheKey getCacheKey();
    }

    /**
     * Key used for cached persistence providers. The key checks
     * the class loader to determine if the persistence providers
     * is a match to the requested one. The loader may be null.
     */
    private class CacheKey implements Cloneable {

        /* Weak Reference to ClassLoader */
        private LoaderReference loaderRef;

        /* Cached Hashcode */
        private int hashCodeCache;

        CacheKey(ClassLoader loader) {
            if (loader == null) {
                this.loaderRef = null;
            } else {
                loaderRef = new LoaderReference(loader, referenceQueue, this);
            }
            calculateHashCode();
        }

        ClassLoader getLoader() {
            return (loaderRef != null) ? loaderRef.get() : null;
        }

        @Override
        public boolean equals(Object other) {
            if (this == other) {
                return true;
            }
            if (other == null || !getClass().equals(other.getClass())) {
                return false;
            }
            final CacheKey otherEntry = (CacheKey) other;
            // quick check to see if they are not equal
            if (hashCodeCache != otherEntry.hashCodeCache) {
                return false;
            }
            // are refs (both non-null) or (both null)?
            if (loaderRef == null) {
                return otherEntry.loaderRef == null;
            }
            ClassLoader loader = loaderRef.get();
            // With a null reference we can no longer find out which class loader was referenced; so treat it as unequal
            return (otherEntry.loaderRef != null) && (loader != null) && (loader == otherEntry.loaderRef.get());
        }

        @Override
        public int hashCode() {
            return hashCodeCache;
        }

        private void calculateHashCode() {
            ClassLoader loader = getLoader();
            if (loader != null) {
                hashCodeCache = loader.hashCode();
            }
        }

        public Object clone() {
            try {
                CacheKey clone = (CacheKey) super.clone();
                if (loaderRef != null) {
                    clone.loaderRef = new LoaderReference(loaderRef.get(), referenceQueue, clone);
                }
                return clone;
            } catch (CloneNotSupportedException e) {
                // this should never happen
                throw new InternalError(e);
            }
        }

        public String toString() {
            return "CacheKey[" + getLoader() + ")]";
        }
    }

    /**
     * References to class loaders are weak references, so that they can be
     * garbage collected when nobody else is using them. The DefaultPersistenceProviderResolver
     * class has no reason to keep class loaders alive.
     */
    private class LoaderReference extends WeakReference<ClassLoader> implements CacheKeyReference {
        private final CacheKey cacheKey;

        @SuppressWarnings("unchecked")
        LoaderReference(ClassLoader referent, ReferenceQueue q, CacheKey key) {
            super(referent, q);
            cacheKey = key;
        }

        public CacheKey getCacheKey() {
            return cacheKey;
        }
    }

    /**
     * References to persistence provider are soft references so that they can be garbage
     * collected when they have no hard references.
     */
    private class PersistenceProviderReference extends SoftReference<List<PersistenceProvider>> implements CacheKeyReference {
        private final CacheKey cacheKey;

        @SuppressWarnings("unchecked")
        PersistenceProviderReference(List<PersistenceProvider> referent, ReferenceQueue q, CacheKey key) {
            super(referent, q);
            cacheKey = key;
        }

        public CacheKey getCacheKey() {
            return cacheKey;
        }
    }
}

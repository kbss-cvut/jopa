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
package cz.cvut.kbss.jopa.utils;

import cz.cvut.kbss.jopa.proxy.lazy.LazyLoadingProxy;
import cz.cvut.kbss.jopa.proxy.lazy.gen.LazyLoadingEntityProxy;

import java.util.Objects;

/**
 * Utilities related to lazy loading.
 */
public class JOPALazyUtils {

    private JOPALazyUtils() {
        throw new AssertionError();
    }

    /**
     * Checks if this specified object is a lazy loading proxy (instance of {@link LazyLoadingProxy}).
     *
     * @param object Object to investigate
     * @return {@code true} if argument is lazy loading proxy, {@code false} otherwise
     */
    public static boolean isLazyLoadingProxy(Object object) {
        return object instanceof LazyLoadingProxy<?>;
    }

    /**
     * Triggers loading on the specified lazy loading proxy.
     * <p>
     * If the specified object is not a {@link cz.cvut.kbss.jopa.proxy.lazy.LazyLoadingProxy} or is a proxy that is
     * already loaded, nothing happens.
     *
     * @param proxy Proxy to load
     */
    public static void load(Object proxy) {
        if (proxy instanceof LazyLoadingProxy<?> p) {
            p.triggerLazyLoading();
        }
    }

    /**
     * Checks whether the specified object has been loaded or is still a lazy loading proxy.
     *
     * @param proxy Proxy to check
     * @return {@code true} if the object is a {@code LazyLoadingProxy} and is not loaded, {@code false} otherwise
     */
    public static boolean isLoaded(Object proxy) {
        return !(proxy instanceof LazyLoadingProxy<?> lazyLoadingProxy) || (lazyLoadingProxy.isLoaded());
    }

    /**
     * Gets the class represented by the specified lazy loading proxy.
     * <p>
     * If the specified object proxies an entity, the corresponding entity class is returned. If the object proxies a
     * collection, the corresponding interface is returned. If the specified object is not a
     * {@link cz.cvut.kbss.jopa.proxy.lazy.LazyLoadingProxy}, its runtime class is returned.
     *
     * @param proxy Proxy whose class to get
     * @return Runtime class unwrapped from a lazy loading proxy
     */
    public static Class<?> getClass(Object proxy) {
        Objects.requireNonNull(proxy);
        return (proxy instanceof LazyLoadingEntityProxy<?> entityProxy) ? entityProxy.getProxiedClass() : proxy.getClass();
    }
}

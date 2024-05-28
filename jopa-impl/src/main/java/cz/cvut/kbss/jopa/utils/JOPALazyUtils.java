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

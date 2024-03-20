package cz.cvut.kbss.jopa.proxy.lazy;

/**
 * Marker interface for lazy loading proxy implementations.
 *
 * @param <T> Type of the loaded value
 */
public interface LazyLoadingProxy<T> {

    /**
     * Triggers value loading.
     *
     * @return Loaded value
     */
    T triggerLazyLoading();
}

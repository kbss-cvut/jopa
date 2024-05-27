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

    /**
     * Checks whether this proxy has already been loaded.
     *
     * @return {@code true} if this proxy has been loaded, {@code false} otherwise
     */
    boolean isLoaded();

    /**
     * If this proxy has already been loaded, this method returns the loaded value.
     *
     * @return The loaded value, {@code null} if this proxy has not been loaded, yet
     * @see #isLoaded()
     * @throws IllegalStateException If called before this proxy is loaded
     */
    T getLoadedValue();

    /**
     * Gets a value that should be used to replace this proxy outside a persistence context.
     *
     * @return Default value to replace this proxy with
     */
    default T unwrap() {
        return null;
    }
}

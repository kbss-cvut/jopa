package cz.cvut.kbss.jopa.proxy.lazy.gen;

import cz.cvut.kbss.jopa.exception.LazyLoadingException;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.proxy.lazy.LazyLoadingProxy;

/**
 * Implemented by generated lazy loading entity proxy classes.
 * <p>
 * It defines the default method for triggering lazy loading.
 *
 * @param <T> Type of the lazily-loaded value
 */
public interface LazyLoadingEntityProxy<T> extends LazyLoadingProxyPropertyAccessor, LazyLoadingProxy<T> {

    @Override
    default T triggerLazyLoading() {
        if (getPersistenceContext() == null || !getPersistenceContext().isActive()) {
            throw new LazyLoadingException("No active persistence context is available in lazy loading proxy for attribute "
                    + getFieldSpec() + " of entity " + getOwner());
        }
        return (T) getPersistenceContext().loadEntityField(getOwner(), (FieldSpecification<? super Object, ?>) getFieldSpec());
    }

    /**
     * Common implementation of {@link Object#toString()}.
     *
     * @return String representation of this proxy
     */
    default String stringify() {
        return getClass().getSimpleName() + "[" + getOwner().getClass()
                                                            .getSimpleName() + "." + getFieldSpec().getName() + "]";
    }
}

package cz.cvut.kbss.jopa.proxy.reference;

import cz.cvut.kbss.jopa.exception.LazyLoadingException;
import cz.cvut.kbss.jopa.exceptions.EntityNotFoundException;
import cz.cvut.kbss.jopa.utils.IdentifierTransformer;

public interface EntityReferenceProxy<T> extends EntityReferenceProxyPropertyAccessor<T> {

    default T triggerLoading() {
        if (isLoaded()) {
            return getValue();
        }
        if (getPersistenceContext() == null || !getPersistenceContext().isActive()) {
            throw new LazyLoadingException("No active persistence context is available to load reference of type " + getType() + " with identifier + " + getIdentifier());
        }
        final T loaded = getPersistenceContext().readObject(getType(), getIdentifier(), getDescriptor());
        if (loaded == null) {
            throw new EntityNotFoundException("Entity '" + getType().getSimpleName() + "' with id " + IdentifierTransformer.stringifyIri(getIdentifier()) + " not found in the repository.");
        }
        setValue(loaded);
        return loaded;
    }

    default boolean isLoaded() {
        return getValue() != null;
    }
}

package cz.cvut.kbss.jopa.proxy.lazy;

import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;

/**
 * Collection proxy that triggers field lazy loading when accessed (and connected to an active persistence context).
 *
 * @param <O> Owner object type
 * @param <T> Wrapped object type
 */
abstract class LazyLoadingCollectionProxy<O, T> {

    protected final transient O owner;
    protected final transient FieldSpecification<? super O, T> fieldSpec;
    protected final transient UnitOfWork persistenceContext;

    public LazyLoadingCollectionProxy(O owner, FieldSpecification<? super O, T> fieldSpec,
                                      UnitOfWork persistenceContext) {
        this.owner = owner;
        this.fieldSpec = fieldSpec;
        this.persistenceContext = persistenceContext;
    }

    public T triggerLazyLoading() {
        return (T) persistenceContext.loadEntityField(owner, fieldSpec);
    }
}

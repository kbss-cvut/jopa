package cz.cvut.kbss.jopa.proxy.lazy;

import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;

import java.util.Set;

public class LazyLoadingSetProxy<O, E> extends LazyLoadingCollectionProxy<O, Set<E>, E> implements Set<E> {
    public LazyLoadingSetProxy(O owner, FieldSpecification<? super O, Set<E>> fieldSpec,
                               UnitOfWork persistenceContext) {
        super(owner, fieldSpec, persistenceContext);
    }
}

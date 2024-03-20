package cz.cvut.kbss.jopa.proxy.lazy;

import cz.cvut.kbss.jopa.exception.LazyLoadingException;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;

import java.util.Collection;
import java.util.Iterator;

/**
 * Collection proxy that triggers field lazy loading when accessed (and connected to an active persistence context).
 *
 * @param <O> Owner object type
 * @param <T> Wrapped object type
 */
abstract class LazyLoadingCollectionProxy<O, T extends Collection<E>, E> implements LazyLoadingProxy<T>, Collection<E> {

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
        if (persistenceContext == null || !persistenceContext.isActive()) {
            throw new LazyLoadingException("No active persistence context is available in lazy loading proxy for attribute "
                    + fieldSpec + " of entity " + owner);
        }
        return (T) persistenceContext.loadEntityField(owner, fieldSpec);
    }

    @Override
    public int size() {
        return triggerLazyLoading().size();
    }

    @Override
    public boolean isEmpty() {
        return triggerLazyLoading().isEmpty();
    }

    @Override
    public boolean contains(Object o) {
        return triggerLazyLoading().contains(o);
    }

    @Override
    public Iterator<E> iterator() {
        return triggerLazyLoading().iterator();
    }

    @Override
    public Object[] toArray() {
        return triggerLazyLoading().toArray();
    }

    @Override
    public <ET> ET[] toArray(ET[] a) {
        return triggerLazyLoading().toArray(a);
    }

    @Override
    public boolean add(E e) {
        return triggerLazyLoading().add(e);
    }

    @Override
    public boolean remove(Object o) {
        return triggerLazyLoading().remove(o);
    }

    @Override
    public boolean containsAll(Collection<?> c) {
        return triggerLazyLoading().containsAll(c);
    }

    @Override
    public boolean addAll(Collection<? extends E> c) {
        return triggerLazyLoading().addAll(c);
    }

    @Override
    public boolean removeAll(Collection<?> c) {
        return triggerLazyLoading().removeAll(c);
    }

    @Override
    public boolean retainAll(Collection<?> c) {
        return triggerLazyLoading().retainAll(c);
    }

    @Override
    public void clear() {
        triggerLazyLoading().clear();
    }


    @Override
    public int hashCode() {
        return triggerLazyLoading().hashCode();
    }

    @Override
    public boolean equals(Object obj) {
        return triggerLazyLoading().equals(obj);
    }
}
package cz.cvut.kbss.jopa.proxy.lazy;

import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;

import java.util.Collection;
import java.util.List;
import java.util.ListIterator;

public class LazyLoadingListProxy<O, E> extends LazyLoadingCollectionProxy<O, List<E>, E> implements List<E> {

    public LazyLoadingListProxy(O owner, FieldSpecification<? super O, List<E>> fieldSpec,
                                UnitOfWork persistenceContext) {
        super(owner, fieldSpec, persistenceContext);
    }

    @Override
    public boolean addAll(int index, Collection<? extends E> c) {
        return triggerLazyLoading().addAll(index, c);
    }

    @Override
    public E get(int index) {
        return triggerLazyLoading().get(index);
    }

    @Override
    public E set(int index, E element) {
        return triggerLazyLoading().set(index, element);
    }

    @Override
    public void add(int index, E element) {
        triggerLazyLoading().add(index, element);
    }

    @Override
    public E remove(int index) {
        return triggerLazyLoading().remove(index);
    }

    @Override
    public int indexOf(Object o) {
        return triggerLazyLoading().indexOf(o);
    }

    @Override
    public int lastIndexOf(Object o) {
        return triggerLazyLoading().lastIndexOf(o);
    }

    @Override
    public ListIterator<E> listIterator() {
        return triggerLazyLoading().listIterator();
    }

    @Override
    public ListIterator<E> listIterator(int index) {
        return triggerLazyLoading().listIterator(index);
    }

    @Override
    public List<E> subList(int fromIndex, int toIndex) {
        return triggerLazyLoading().subList(fromIndex, toIndex);
    }
}

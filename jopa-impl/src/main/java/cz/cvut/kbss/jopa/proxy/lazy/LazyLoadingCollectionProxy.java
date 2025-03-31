/*
 * JOPA
 * Copyright (C) 2025 Czech Technical University in Prague
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

    protected final O owner;
    protected final FieldSpecification<? super O, T> fieldSpec;
    protected final UnitOfWork persistenceContext;

    private T value;

    public LazyLoadingCollectionProxy(O owner, FieldSpecification<? super O, T> fieldSpec,
                                      UnitOfWork persistenceContext) {
        this.owner = owner;
        this.fieldSpec = fieldSpec;
        this.persistenceContext = persistenceContext;
    }

    @Override
    public T triggerLazyLoading() {
        if (value != null) {
            return value;
        }
        if (persistenceContext == null || !persistenceContext.isActive()) {
            throw new LazyLoadingException("No active persistence context is available in lazy loading proxy for attribute "
                    + fieldSpec + " of entity " + owner);
        }
        this.value = (T) persistenceContext.loadEntityField(owner, fieldSpec);
        return value;
    }

    @Override
    public boolean isLoaded() {
        return value != null;
    }

    @Override
    public T getLoadedValue() {
        if (value == null) {
            throw new IllegalStateException("Proxy has not been loaded, yet.");
        }
        return value;
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
    public String toString() {
        return getClass().getSimpleName() + "[" + owner.getClass().getSimpleName() + "." + fieldSpec.getName() + "]";
    }
}

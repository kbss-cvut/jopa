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

import cz.cvut.kbss.jopa.model.metamodel.CollectionType;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;
import cz.cvut.kbss.jopa.utils.CollectionFactory;

import java.util.Collection;
import java.util.List;
import java.util.ListIterator;

public class LazyLoadingListProxy<O, E> extends LazyLoadingCollectionProxy<O, List<E>, E> implements List<E> {

    public LazyLoadingListProxy(O owner, FieldSpecification<? super O, List<E>> fieldSpec,
                                UnitOfWork persistenceContext) {
        super(owner, fieldSpec, persistenceContext);
    }

    @Override
    public List<E> unwrap() {
        return (List<E>) CollectionFactory.createDefaultCollection(CollectionType.LIST);
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

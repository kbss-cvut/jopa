/*
 * JOPA
 * Copyright (C) 2024 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.proxy.change;

import cz.cvut.kbss.jopa.sessions.UnitOfWork;

import java.lang.reflect.Field;
import java.util.Collection;
import java.util.Iterator;
import java.util.Objects;
import java.util.Set;

public class ChangeTrackingIndirectSet<E> extends ChangeTrackingIndirectCollection<Set<E>> implements Set<E> {

    private final Set<E> internalSet;

    public ChangeTrackingIndirectSet(Object owner, Field f, UnitOfWork uow, Set<E> referencedSet) {
        super(owner, f, uow);
        this.internalSet = Objects.requireNonNull(referencedSet);
    }

    @Override
    public int size() {
        return internalSet.size();
    }

    @Override
    public boolean isEmpty() {
        return internalSet.isEmpty();
    }

    @Override
    public boolean contains(Object o) {
        return internalSet.contains(o);
    }

    @Override
    public Iterator<E> iterator() {
        return new IndirectSetIterator<>(internalSet.iterator());
    }

    @Override
    public Object[] toArray() {
        return internalSet.toArray();
    }

    @Override
    public <T> T[] toArray(T[] a) {
        return internalSet.toArray(a);
    }

    @Override
    public boolean add(E e) {
        boolean res = internalSet.add(e);
        if (res) {
            persistChange();
        }
        return res;
    }

    @Override
    public boolean remove(Object o) {
        boolean res = internalSet.remove(o);
        if (res) {
            persistChange();
        }
        return res;
    }

    @Override
    public boolean containsAll(Collection<?> c) {
        return internalSet.containsAll(c);
    }

    @Override

    public boolean addAll(Collection<? extends E> c) {
        boolean res = internalSet.addAll(c);
        if (res) {
            persistChange();
        }
        return res;
    }

    @Override
    public boolean retainAll(Collection<?> c) {
        boolean res = internalSet.retainAll(c);
        if (res) {
            persistChange();
        }
        return res;
    }

    @Override
    public boolean removeAll(Collection<?> c) {
        boolean res = internalSet.removeAll(c);
        if (res) {
            persistChange();
        }
        return res;
    }

    @Override
    public void clear() {
        internalSet.clear();
        persistChange();
    }

    private class IndirectSetIterator<T> implements Iterator<T> {

        private final Iterator<T> iterator;

        private IndirectSetIterator(Iterator<T> iterator) {
            this.iterator = iterator;
        }

        @Override
        public boolean hasNext() {
            return iterator.hasNext();
        }

        @Override
        public T next() {
            return iterator.next();
        }

        @Override
        public void remove() {
            iterator.remove();
            ChangeTrackingIndirectSet.this.persistChange();
        }
    }

    @Override
    public Set<E> unwrap() {
        return internalSet;
    }

    @Override
    public boolean equals(Object o) {
        if (o instanceof Set) {
            if (o instanceof ChangeTrackingIndirectSet) {
                return internalSet.equals(((ChangeTrackingIndirectSet) o).internalSet);
            }
            return internalSet.equals(o);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return internalSet.hashCode();
    }

    @Override
    public String toString() {
        return internalSet.toString();
    }
}

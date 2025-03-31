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
package cz.cvut.kbss.jopa.proxy.change;

import cz.cvut.kbss.jopa.sessions.UnitOfWork;

import java.lang.reflect.Field;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.Objects;

public class ChangeTrackingIndirectList<E> extends ChangeTrackingIndirectCollection<List<E>> implements List<E> {

    private final List<E> internalList;

    /**
     * Create new indirect list backed by the specified referenced list.
     *
     * @param owner          Owner of the list
     * @param f              The field holding this list
     * @param uow            Persistence context the owner belongs to
     * @param referencedList The list to reference
     * @throws NullPointerException If the {@code referencedList} is null
     */
    public ChangeTrackingIndirectList(Object owner, Field f, UnitOfWork uow, List<E> referencedList) {
        super(owner, f, uow);
        this.internalList = Objects.requireNonNull(referencedList);
    }

    @Override
    public boolean add(E arg0) {
        internalList.add(arg0);
        persistChange();    // There is always a change
        return true;
    }

    @Override
    public void add(int arg0, E arg1) {
        internalList.add(arg0, arg1);
        persistChange();
    }

    @Override
    public boolean addAll(Collection<? extends E> c) {
        internalList.addAll(c);
        persistChange();
        return true;
    }

    @Override
    public boolean addAll(int index, Collection<? extends E> c) {
        boolean res = internalList.addAll(index, c);
        if (res) {
            persistChange();
        }
        return res;
    }

    @Override
    public void clear() {
        internalList.clear();
        persistChange();
    }

    @Override
    public boolean contains(Object arg0) {
        return internalList.contains(arg0);
    }

    @Override
    public boolean containsAll(Collection<?> arg0) {
        return internalList.containsAll(arg0);
    }

    @Override
    public E get(int arg0) {
        return internalList.get(arg0);
    }

    @Override
    public int indexOf(Object arg0) {
        return internalList.indexOf(arg0);
    }

    @Override
    public boolean isEmpty() {
        return internalList.isEmpty();
    }

    @Override
    public Iterator<E> iterator() {
        return new IndirectIterator(internalList.iterator());
    }

    @Override
    public int lastIndexOf(Object arg0) {
        return internalList.lastIndexOf(arg0);
    }

    @Override
    public ListIterator<E> listIterator() {
        return new IndirectListIterator(internalList.listIterator());
    }

    @Override
    public ListIterator<E> listIterator(int arg0) {
        return new IndirectListIterator(internalList.listIterator(arg0));
    }

    @Override
    public boolean remove(Object arg0) {
        boolean res = internalList.remove(arg0);
        if (res) {
            persistChange();
        }
        return res;
    }

    @Override
    public E remove(int arg0) {
        E elem = internalList.remove(arg0);
        persistChange();
        return elem;
    }

    @Override
    public boolean removeAll(Collection<?> arg0) {
        boolean res = internalList.removeAll(arg0);
        if (res) {
            persistChange();
        }
        return res;
    }

    @Override
    public boolean retainAll(Collection<?> arg0) {
        boolean res = internalList.retainAll(arg0);
        if (res) {
            persistChange();
        }
        return res;
    }

    @Override
    public E set(int arg0, E arg1) {
        E elem = internalList.set(arg0, arg1);
        persistChange();
        return elem;
    }

    @Override
    public int size() {
        return internalList.size();
    }

    @Override
    public List<E> subList(int fromIndex, int toIndex) {
        return new ChangeTrackingIndirectList<>(owner, field, persistenceContext, internalList.subList(fromIndex, toIndex));
    }

    @Override
    public Object[] toArray() {
        return internalList.toArray();
    }

    @Override
    public <T> T[] toArray(T[] arg0) {
        return internalList.toArray(arg0);
    }

    @Override
    public List<E> unwrap() {
        return internalList;
    }

    @Override
    public boolean equals(Object o) {
        if (o instanceof List) {
            if (o instanceof ChangeTrackingIndirectList) {
                return internalList.equals(((ChangeTrackingIndirectList) o).internalList);
            }
            return internalList.equals(o);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return internalList.hashCode();
    }

    @Override
    public String toString() {
        return internalList.toString();
    }

    private class IndirectIterator implements Iterator<E> {

        private final Iterator<E> it;

        private IndirectIterator(Iterator<E> it) {
            this.it = it;
        }

        @Override
        public boolean hasNext() {
            return it.hasNext();
        }

        @Override
        public E next() {
            return it.next();
        }

        @Override
        public void remove() {
            it.remove();
            ChangeTrackingIndirectList.this.persistChange();
        }
    }

    private class IndirectListIterator implements ListIterator<E> {

        private final ListIterator<E> lit;

        private IndirectListIterator(ListIterator<E> lit) {
            this.lit = lit;
        }

        @Override
        public boolean hasNext() {
            return lit.hasNext();
        }

        @Override
        public E next() {
            return lit.next();
        }

        @Override
        public boolean hasPrevious() {
            return lit.hasPrevious();
        }

        @Override
        public E previous() {
            return lit.previous();
        }

        @Override
        public int nextIndex() {
            return lit.nextIndex();
        }

        @Override
        public int previousIndex() {
            return lit.previousIndex();
        }

        @Override
        public void remove() {
            lit.remove();
            ChangeTrackingIndirectList.this.persistChange();
        }

        @Override
        public void set(E e) {
            lit.set(e);
            ChangeTrackingIndirectList.this.persistChange();
        }

        @Override
        public void add(E e) {
            lit.add(e);
            ChangeTrackingIndirectList.this.persistChange();
        }
    }
}

package cz.cvut.kbss.jopa.adapters;

import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import cz.cvut.kbss.jopa.sessions.UnitOfWorkImpl;

public class IndirectSet<E> extends IndirectCollection implements Set<E> {

	private Set<E> internalSet;

	/**
	 * Private constructor to allow clone building.
	 */
	private IndirectSet() {
		super();
		this.internalSet = new HashSet<E>();
	}

	public IndirectSet(Object owner, UnitOfWorkImpl uow, Set<E> referencedSet) {
		super(owner, uow);
		if (referencedSet == null) {
			throw new NullPointerException("Null passed in as the referencedSet.");
		}
		this.internalSet = referencedSet;
	}

	public int size() {
		return internalSet.size();
	}

	public boolean isEmpty() {
		return internalSet.isEmpty();
	}

	public boolean contains(Object o) {
		return internalSet.contains(o);
	}

	public Iterator<E> iterator() {
		return new IndirectSetIterator<E>(internalSet.iterator());
	}

	public Object[] toArray() {
		return internalSet.toArray();
	}

	public <T> T[] toArray(T[] a) {
		return internalSet.toArray(a);
	}

	public boolean add(E e) {
		boolean res = internalSet.add(e);
		if (res) {
			persistChange();
		}
		return res;
	}

	public boolean remove(Object o) {
		boolean res = internalSet.remove(o);
		if (res) {
			persistChange();
		}
		return res;
	}

	public boolean containsAll(Collection<?> c) {
		return internalSet.containsAll(c);
	}

	public boolean addAll(Collection<? extends E> c) {
		boolean res = internalSet.addAll(c);
		if (res) {
			persistChange();
		}
		return res;
	}

	public boolean retainAll(Collection<?> c) {
		boolean res = internalSet.retainAll(c);
		if (res) {
			persistChange();
		}
		return res;
	}

	public boolean removeAll(Collection<?> c) {
		boolean res = internalSet.removeAll(c);
		if (res) {
			persistChange();
		}
		return res;
	}

	public void clear() {
		internalSet.clear();
		persistChange();
	}

	private class IndirectSetIterator<T> implements Iterator<T> {

		private Iterator<T> iterator;

		private IndirectSetIterator(Iterator<T> iterator) {
			this.iterator = iterator;
		}

		public boolean hasNext() {
			return iterator.hasNext();
		}

		public T next() {
			return iterator.next();
		}

		public void remove() {
			iterator.remove();
			IndirectSet.this.persistChange();
		}
	}

	@Override
	public Collection<?> getReferencedCollection() {
		return internalSet;
	}

	@Override
	public boolean equals(Object o) {
		return internalSet.equals(o);
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

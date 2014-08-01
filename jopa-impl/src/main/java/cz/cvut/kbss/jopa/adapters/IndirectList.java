package cz.cvut.kbss.jopa.adapters;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;

import cz.cvut.kbss.jopa.sessions.UnitOfWorkImpl;

public class IndirectList<E> extends IndirectCollection<List<E>> implements List<E> {

	private final List<E> internalList;

	/**
	 * No-arg constructor to allow clone building.
	 */
	IndirectList() {
		super();
		this.internalList = new ArrayList<E>();
	}

	/**
	 * Create new indirect list backed by the specified referenced list.
	 * 
	 * @param owner
	 *            Owner of the list
	 * @param f
	 *            The field holding this list
	 * @param uow
	 *            Persistence context the owner belongs to
	 * @param referencedList
	 *            The list to reference
	 * @throws NullPointerException
	 *             If the {@code referencedList} is null
	 */
	public IndirectList(Object owner, Field f, UnitOfWorkImpl uow, List<E> referencedList) {
		super(owner, f, uow);
		if (referencedList == null) {
			throw new NullPointerException("Null passed in as the referencedList.");
		}
		this.internalList = referencedList;
	}

	public boolean add(E arg0) {
		boolean res = internalList.add(arg0);
		if (res) {
			persistChange();
		}
		return res;
	}

	public void add(int arg0, E arg1) {
		internalList.add(arg0, arg1);
		persistChange();
	}

	public boolean addAll(Collection<? extends E> arg0) {
		boolean res = internalList.addAll(arg0);
		if (res) {
			persistChange();
		}
		return res;
	}

	public boolean addAll(int arg0, Collection<? extends E> arg1) {
		boolean res = internalList.addAll(arg0, arg1);
		if (res) {
			persistChange();
		}
		return res;
	}

	public void clear() {
		internalList.clear();
		persistChange();
	}

	public boolean contains(Object arg0) {
		return internalList.contains(arg0);
	}

	public boolean containsAll(Collection<?> arg0) {
		return internalList.containsAll(arg0);
	}

	public E get(int arg0) {
		return internalList.get(arg0);
	}

	public int indexOf(Object arg0) {
		return internalList.indexOf(arg0);
	}

	public boolean isEmpty() {
		return internalList.isEmpty();
	}

	public Iterator<E> iterator() {
		return new IndirectIterator(internalList.iterator());
	}

	public int lastIndexOf(Object arg0) {
		return internalList.lastIndexOf(arg0);
	}

	public ListIterator<E> listIterator() {
		return new IndirectListIterator(internalList.listIterator());
	}

	public ListIterator<E> listIterator(int arg0) {
		return new IndirectListIterator(internalList.listIterator(arg0));
	}

	public boolean remove(Object arg0) {
		boolean res = internalList.remove(arg0);
		if (res) {
			persistChange();
		}
		return res;
	}

	public E remove(int arg0) {
		E elem = internalList.remove(arg0);
		persistChange();
		return elem;
	}

	public boolean removeAll(Collection<?> arg0) {
		boolean res = internalList.removeAll(arg0);
		if (res) {
			persistChange();
		}
		return res;
	}

	public boolean retainAll(Collection<?> arg0) {
		boolean res = internalList.retainAll(arg0);
		if (res) {
			persistChange();
		}
		return res;
	}

	public E set(int arg0, E arg1) {
		E elem = internalList.set(arg0, arg1);
		persistChange();
		return elem;
	}

	public int size() {
		return internalList.size();
	}

	public List<E> subList(int arg0, int arg1) {
		return new IndirectList<E>(owner, field, persistenceContext, internalList.subList(arg0,
				arg1));
	}

	public Object[] toArray() {
		return internalList.toArray();
	}

	public <T> T[] toArray(T[] arg0) {
		return internalList.toArray(arg0);
	}

	@Override
	public List<E> getReferencedCollection() {
		return internalList;
	}

	@Override
	public boolean equals(Object o) {
		return internalList.equals(o);
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
			IndirectList.this.persistChange();
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
			IndirectList.this.persistChange();
		}

		@Override
		public void set(E e) {
			lit.set(e);
			IndirectList.this.persistChange();
		}

		@Override
		public void add(E e) {
			lit.add(e);
			IndirectList.this.persistChange();
		}
	}
}

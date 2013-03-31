package cz.cvut.kbss.jopa.adapters;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;

import cz.cvut.kbss.jopa.sessions.UnitOfWorkImpl;

public class IndirectList<E> extends IndirectCollection implements List<E> {

	private final List<E> internalList;

	/**
	 * Private constructor to allow clone building.
	 */
	private IndirectList() {
		super();
		this.internalList = new ArrayList<E>();
	}

	/**
	 * Create new indirect list backed by the specified referenced list.
	 * 
	 * @param owner
	 *            Owner of the list
	 * @param uow
	 *            Persistence context the owner belongs to
	 * @param referencedList
	 *            The list to reference
	 * @throws NullPointerException
	 *             If the {@code referencedList} is null
	 */
	public IndirectList(Object owner, UnitOfWorkImpl uow, List<E> referencedList) {
		super(owner, uow);
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
		return internalList.iterator();
	}

	public int lastIndexOf(Object arg0) {
		return internalList.lastIndexOf(arg0);
	}

	public ListIterator<E> listIterator() {
		return internalList.listIterator();
	}

	public ListIterator<E> listIterator(int arg0) {
		return internalList.listIterator(arg0);
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
		return new IndirectList<E>(owner, persistenceContext, internalList.subList(arg0, arg1));
	}

	public Object[] toArray() {
		return internalList.toArray();
	}

	public <T> T[] toArray(T[] arg0) {
		return internalList.toArray(arg0);
	}

	@Override
	public Collection<?> getReferencedCollection() {
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
}

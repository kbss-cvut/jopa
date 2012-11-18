package cz.cvut.kbss.jopa.sessions;

import java.lang.reflect.Field;
import java.util.Collection;
import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;

import cz.cvut.kbss.jopa.model.OWLInferredAttributeModifiedException;
import cz.cvut.kbss.jopa.model.OWLPersistenceException;
import cz.cvut.kbss.jopa.sessions.ChangeManager;
import cz.cvut.kbss.jopa.sessions.ChangeRecord;
import cz.cvut.kbss.jopa.sessions.ObjectChangeSet;

public class ChangeManagerImpl implements ChangeManager {

	private static final Logger LOG = Logger.getLogger(ChangeManagerImpl.class.getName());
	private Map<Object, Object> visitedObjects;

	public ChangeManagerImpl() {
		visitedObjects = new IdentityHashMap<Object, Object>();
	}

	public boolean hasChanges(Object original, Object clone) {
		LOG.config("Checking for changes...");
		boolean res = hasChangesInternal(original, clone);
		visitedObjects.clear();
		return res;
	}

	/**
	 * This method does the actual check for changes. It is wrapped in the
	 * public method since the IdentityMap for visited objects has to be cleared
	 * after the whole check is done.
	 * 
	 * @param original
	 *            The original object.
	 * @param clone
	 *            The clone that may have changed.
	 * @return True if the clone is in different state than the original.
	 */
	protected boolean hasChangesInternal(Object original, Object clone) {
		if (clone == null && original == null) {
			return false;
		}
		if (clone == null && original != null || clone != null && original == null) {
			return true;
		}
		if (visitedObjects.containsKey(clone)) {
			return false;
		}
		boolean changes = false;
		final Class<?> cls = clone.getClass();
		List<Field> fields = CloneBuilderImpl.getAllFields(cls);
		Map<Object, Object> composedObjects = new HashMap<Object, Object>();
		Iterator<Field> it = fields.iterator();
		try {
			while (it.hasNext()) {
				Field f = it.next();
				if (!f.isAccessible()) {
					f.setAccessible(true);
				}
				Object clVal = f.get(clone);
				Object origVal = f.get(original);
				if ((clVal == null && origVal != null) || (clVal != null && origVal == null)) {
					changes = true;
					break;
				}
				if (clVal == null && origVal == null) {
					continue;
				}
				if (CloneBuilderImpl.isPrimitiveOrString(clVal.getClass())) {
					if (!clVal.equals(origVal)) {
						changes = true;
						break;
					} else {
						continue;
					}
				} else if (clVal instanceof Collection) {
					changes = hasCollectionChanged(clVal, origVal);
				} else {
					visitedObjects.put(clVal, clVal);
					composedObjects.put(clVal, origVal);
				}
			}
			// First check all primitive values - performance, then do composed
			for (Object cl : composedObjects.keySet()) {
				if (hasChangesInternal(cl, composedObjects.get(cl))) {
					changes = true;
					break;
				}
			}
		} catch (IllegalAccessException e) {
			throw new OWLPersistenceException(
					"Exception caught when trying to check changes on entities.", e);
		}
		return changes;
	}

	/**
	 * This method checks for changes in collections. A change may be different
	 * size - removed or added element, or different elements or even different
	 * order of elements.
	 * 
	 * @param clone
	 *            The cloned collection.
	 * @param original
	 *            The original collection.
	 * @return True if the clone collection contains any modifications compared
	 *         to the original collection.
	 */
	private boolean hasCollectionChanged(Object clone, Object original) {
		boolean hasChanged = false;
		Collection<?> origCol = (Collection<?>) original;
		Collection<?> cloneCol = (Collection<?>) clone;
		if (origCol.size() != cloneCol.size()) {
			return true;
		}
		Iterator<?> itOrig = origCol.iterator();
		Iterator<?> itClone = cloneCol.iterator();
		while (itOrig.hasNext() && itClone.hasNext() && hasChanged == false) {
			Object cl = itClone.next();
			Object orig = itOrig.next();
			hasChanged = hasChangesInternal(orig, cl);
		}
		return hasChanged;
	}

	public ObjectChangeSet calculateChanges(ObjectChangeSet changeSet)
			throws IllegalAccessException, IllegalArgumentException,
			OWLInferredAttributeModifiedException {
		if (changeSet == null) {
			return null;
		}
		return this.calculateChangesInternal(changeSet);
	}

	/**
	 * This internal method does the actual changes calculation. It compares
	 * every non-static attribute of the clone to the original value. If the
	 * values are different, a change record is added into the change set.
	 * 
	 * @param changeSet
	 *            The change set where change records will be put in. It also
	 *            contains reference to the clone and original object.
	 * @return Change set containing change records or null if there were no
	 *         changes.
	 * @throws IllegalArgumentException
	 * @throws IllegalAccessException
	 * @throws OWLInferredAttributeModifiedException
	 */
	protected ObjectChangeSet calculateChangesInternal(ObjectChangeSet changeSet)
			throws IllegalArgumentException, IllegalAccessException,
			OWLInferredAttributeModifiedException {
		Object original = changeSet.getChangedObject();
		Object clone = changeSet.getCloneObject();
		final List<Field> fields = CloneBuilderImpl.getAllFields(clone.getClass());
		boolean changes = false;
		for (Field f : fields) {
			if (!f.isAccessible()) {
				f.setAccessible(true);
			}
			Object clVal = f.get(clone);
			Object origVal = f.get(original);
			ChangeRecord r = null;
			if (clVal == null && origVal == null) {
				continue;
			}
			final String attName = f.getName();
			if (clVal == null && origVal != null) {
				r = new ChangeRecordImpl(attName, null);
				changes = true;
			} else if (clVal != null && origVal == null) {
				r = new ChangeRecordImpl(attName, clVal);
				changes = true;
			} else {
				if (CloneBuilderImpl.isPrimitiveOrString(clVal.getClass())) {
					if (!clVal.equals(origVal)) {
						changes = true;
						r = new ChangeRecordImpl(attName, clVal);
					} else {
						continue;
					}
				} else if (clVal instanceof Collection) {
					if (hasCollectionChanged(origVal, clVal)) {
						changes = true;
						r = new ChangeRecordImpl(attName, clVal);
					} else {
						continue;
					}
				} else {
					if (hasChanges(origVal, clVal)) {
						changes = true;
						r = new ChangeRecordImpl(attName, clVal);
					} else {
						continue;
					}
				}
			}
			if (changes && CloneBuilderImpl.isFieldInferred(f)) {
				throw new OWLInferredAttributeModifiedException(
						"Modifying inferred attributes is forbidden.");
			}
			changeSet.addChangeRecord(r);
		}
		if (!changes) {
			return null;
		} else {
			return changeSet;
		}
	}

}

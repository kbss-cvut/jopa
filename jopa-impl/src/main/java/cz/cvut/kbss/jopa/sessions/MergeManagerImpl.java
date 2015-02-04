package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;

import java.net.URI;

public class MergeManagerImpl implements MergeManager {

	// The UnitOfWork instance for this MergeManager
	protected UnitOfWorkImpl uow;

	protected CloneBuilder builder;

	public MergeManagerImpl(UnitOfWorkImpl session) {
		this.uow = session;
		this.builder = new CloneBuilderImpl(session);
	}

	protected void deleteObjectFromCache(ObjectChangeSet changeSet) {
		Object original = changeSet.getChangedObject();
		if (original != null) {
			uow.removeObjectFromCache(original, changeSet.getEntityContext());
		}
	}

	public Object mergeChangesOnObject(Object clone, ObjectChangeSet changeSet) {
		if (changeSet == null) {
			throw new OWLPersistenceException("Change Set in Merge Manager null.");
		}
		if (clone == null) {
			return null;
		}

		Object original = changeSet.getChangedObject();
		if (original == null) {
			// If the original is null, then we may have a new object
			// but this should not happen since new objects are handled
			// separately
			if (uow.isObjectNew(clone)) {
				mergeNewObject(changeSet);
			} else {
				throw new OWLPersistenceException("Cannot find the original object.");
			}
		} else {
			this.builder.mergeChanges(original, changeSet);
		}
		return clone;
	}

	public void mergeChangesFromChangeSet(UnitOfWorkChangeSet changeSet) {
		for (ObjectChangeSet objectChangeSet : changeSet.getExistingObjectsChanges()) {
				Object clone = objectChangeSet.getCloneObject();
				mergeChangesOnObject(clone, objectChangeSet);
		}
		for (ObjectChangeSet objectChangeSet : changeSet.getNewObjects()) {
				mergeNewObject(objectChangeSet);
		}
		for (ObjectChangeSet deletedChangeSet : changeSet.getDeletedObjects()) {
			deleteObjectFromCache(deletedChangeSet);
		}

	}

	public void mergeNewObject(ObjectChangeSet changeSet) {
		if (changeSet == null) {
			return;
		}
		Object clone = changeSet.getCloneObject();
		if (!changeSet.isNew()) {
			mergeChangesOnObject(clone, changeSet);
		}
		// Put the original object into the shared session cache
		Object newObject = changeSet.getChangedObject();
		final Object primaryKey = EntityPropertiesUtils.getPrimaryKey(newObject, uow.getMetamodel());
		final URI context = changeSet.getEntityContext();
		uow.putObjectIntoCache(primaryKey, newObject, context);
	}

}

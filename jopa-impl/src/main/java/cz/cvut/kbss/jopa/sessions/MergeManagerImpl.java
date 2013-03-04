package cz.cvut.kbss.jopa.sessions;

import java.util.Iterator;
import java.util.Map;

import org.semanticweb.owlapi.model.IRI;

import cz.cvut.kbss.jopa.model.OWLPersistenceException;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;

public class MergeManagerImpl implements MergeManager {

	// The UnitOfWork instance for this MergeManager
	protected AbstractSession session;

	protected CloneBuilder builder;

	public MergeManagerImpl(AbstractSession session) {
		this.session = session;
		this.builder = new CloneBuilderImpl((UnitOfWorkImpl) session);
	}

	protected void deleteObjectFromCache(ObjectChangeSet changeSet) {
		Object original = changeSet.getChangedObject();
		if (original != null) {
			session.removeObjectFromCache(original);
		}
	}

	public Object mergeChangesOnObject(Object clone, ObjectChangeSet changeSet) {
		if (changeSet == null) {
			throw new OWLPersistenceException(
					"Change Set in Merge Manager null.");
		}
		if (clone == null) {
			return clone;
		}
		UnitOfWorkImpl unitOfWork = (UnitOfWorkImpl) this.session;

		Object original = changeSet.getChangedObject();
		if (original == null) {
			// If the original is null, then we may have a new object
			// but this should not happen since new objects are handled
			// separately
			if (unitOfWork.isObjectNew(clone)) {
				mergeNewObject(changeSet);
			} else {
				throw new OWLPersistenceException(
						"Cannot find the original object.");
			}
		} else {
			this.builder.mergeChanges(original, clone, changeSet, this);
		}
		return clone;
	}

	public void mergeChangesFromChangeSet(UnitOfWorkChangeSet changeSet) {
		Iterator<?> objectChangeIterator = changeSet.getObjectChanges()
				.keySet().iterator();
		while (objectChangeIterator.hasNext()) {
			Map<?, ?> changeSets = (Map<?, ?>) changeSet.getObjectChanges()
					.get(objectChangeIterator.next());
			Iterator<?> mapIterator = changeSets.keySet().iterator();
			while (mapIterator.hasNext()) {
				ObjectChangeSet objectChangeSet = (ObjectChangeSet) mapIterator
						.next();
				Object clone = objectChangeSet.getCloneObject();
				mergeChangesOnObject(clone, objectChangeSet);
			}
		}
		Iterator<?> newObjectsIterator = changeSet.getNewObjectChangeSets()
				.keySet().iterator();
		while (newObjectsIterator.hasNext()) {
			Map<?, ?> changeSetsForCls = (Map<?, ?>) changeSet
					.getNewObjectChangeSets().get(newObjectsIterator.next());
			Iterator<?> chsIterator = changeSetsForCls.keySet().iterator();
			while (chsIterator.hasNext()) {
				ObjectChangeSet objectChangeSet = (ObjectChangeSet) changeSetsForCls
						.get(chsIterator.next());
				mergeNewObject(objectChangeSet);
			}
		}
		if (changeSet.hasDeletedObjects()) {
			Iterator<?> deletedObjectsIterator = changeSet.getDeletedObjects()
					.keySet().iterator();
			while (deletedObjectsIterator.hasNext()) {
				ObjectChangeSet deletedChangeSet = (ObjectChangeSet) deletedObjectsIterator
						.next();
				deleteObjectFromCache(deletedChangeSet);
			}
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
		final IRI primaryKey = EntityPropertiesUtils.getPrimaryKey(newObject,
				session.getMetamodel());
		session.getLiveObjectCache().acquireWriteLock();
		session.getLiveObjectCache().add(primaryKey, newObject);
		session.getLiveObjectCache().releaseWriteLock();
	}

}

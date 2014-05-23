package cz.cvut.kbss.jopa.sessions;

import java.io.Serializable;
import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.Map;

public class UnitOfWorkChangeSetImpl implements Serializable, UnitOfWorkChangeSet {

	private static final long serialVersionUID = 7834438138173201896L;
	// Keeps all the change sets. This is here only for future serialization
	// support
	protected Map<ObjectChangeSet, ObjectChangeSet> allchangeSets;
	protected Map<ObjectChangeSet, ObjectChangeSet> deletedObjects;

	protected transient Map<Class<?>, Map<ObjectChangeSet, ObjectChangeSet>> objectChanges;
	protected transient Map<Class<?>, Map<ObjectChangeSet, ObjectChangeSet>> newObjectChanges;

	protected boolean hasChanges;

	public UnitOfWorkChangeSetImpl() {
		super();
		this.objectChanges = new HashMap<Class<?>, Map<ObjectChangeSet, ObjectChangeSet>>();
		this.deletedObjects = new IdentityHashMap<ObjectChangeSet, ObjectChangeSet>();
		this.newObjectChanges = new HashMap<Class<?>, Map<ObjectChangeSet, ObjectChangeSet>>();
		this.allchangeSets = new IdentityHashMap<ObjectChangeSet, ObjectChangeSet>();
	}

	@Override
	public void addObjectChangeSet(ObjectChangeSet objectChangeSet) {
		if (objectChangeSet.isNew()) {
			this.addNewObjectChangeSet(objectChangeSet);
		} else {
			this.hasChanges = true;
			Map<ObjectChangeSet, ObjectChangeSet> map = getObjectChanges().get(
					objectChangeSet.getObjectClass());
			if (map == null) {
				map = new HashMap<ObjectChangeSet, ObjectChangeSet>();
				getObjectChanges().put(objectChangeSet.getObjectClass(), map);
			}
			map.put(objectChangeSet, objectChangeSet);
		}
		getAllChangeSets().put(objectChangeSet, objectChangeSet);
	}

	@Override
	public void addDeletedObject(ObjectChangeSet deletedObject) {
		getDeletedObjects().put(deletedObject, deletedObject);
	}

	@Override
	public void addNewObjectChangeSet(ObjectChangeSet newObject) {
		newObject.setNew(true);
		Map<ObjectChangeSet, ObjectChangeSet> changeSets = getNewObjectChangeSets().get(
				newObject.getObjectClass());
		if (changeSets == null) {
			// EL uses IdentityHashMap
			changeSets = new IdentityHashMap<ObjectChangeSet, ObjectChangeSet>();
			getNewObjectChangeSets().put(newObject.getObjectClass(), changeSets);
		}
		changeSets.put(newObject, newObject);
		getAllChangeSets().put(newObject, newObject);
		this.hasChanges = true;
	}

	@Override
	public Map<Class<?>, Map<ObjectChangeSet, ObjectChangeSet>> getObjectChanges() {
		return this.objectChanges;
	}

	@Override
	public Map<ObjectChangeSet, ObjectChangeSet> getDeletedObjects() {
		return this.deletedObjects;
	}

	@Override
	public Map<Class<?>, Map<ObjectChangeSet, ObjectChangeSet>> getNewObjectChangeSets() {
		return this.newObjectChanges;
	}

	public Map<ObjectChangeSet, ObjectChangeSet> getAllChangeSets() {
		return this.allchangeSets;
	}

	@Override
	public void removeObjectChangeSet(ObjectChangeSet changeSet) {
		if (changeSet == null)
			return;
		Map<?, ?> classChanges = getObjectChanges().get(changeSet.getObjectClass());
		if (classChanges != null) {
			classChanges.remove(changeSet);
			if (classChanges.isEmpty()) {
				getObjectChanges().remove(changeSet.getObjectClass());
			}
		}
		getAllChangeSets().remove(changeSet);
		if (getAllChangeSets().isEmpty()) {
			this.hasChanges = false;
		}
	}

	@Override
	public boolean hasDeletedObjects() {
		return !deletedObjects.isEmpty();
	}

	@Override
	public boolean hasChanges() {
		return (this.hasChanges || hasDeletedObjects());
	}

	@Override
	public boolean hasNew() {
		return !this.newObjectChanges.isEmpty();
	}

}

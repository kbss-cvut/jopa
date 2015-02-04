package cz.cvut.kbss.jopa.sessions;

import java.io.Serializable;
import java.util.*;

public class UnitOfWorkChangeSetImpl implements Serializable, UnitOfWorkChangeSet {

	private static final long serialVersionUID = 7834438138173201896L;

	private Set<ObjectChangeSet> deletedObjects;
	private Map<Object, ObjectChangeSet> objectChanges;
	private Set<ObjectChangeSet> newObjectChanges;

	public UnitOfWorkChangeSetImpl() {
		super();
		this.objectChanges = new HashMap<>();
		this.deletedObjects = new HashSet<>();
		this.newObjectChanges = new HashSet<>();
	}

	@Override
	public void addObjectChangeSet(ObjectChangeSet objectChangeSet) {
		if (objectChangeSet.isNew()) {
			addNewObjectChangeSet(objectChangeSet);
		} else {
			objectChanges.put(objectChangeSet.getChangedObject(), objectChangeSet);
		}
	}

	@Override
	public void addDeletedObjectChangeSet(ObjectChangeSet deletedObject) {
		deletedObjects.add(deletedObject);
	}

	@Override
	public void addNewObjectChangeSet(ObjectChangeSet newObject) {
		newObject.setNew(true);
		newObjectChanges.add(newObject);
	}

	@Override
	public Collection<ObjectChangeSet> getExistingObjectsChanges() {
		return Collections.unmodifiableCollection(objectChanges.values());
    }

    @Override
    public ObjectChangeSet getExistingObjectChanges(Object original) {
        return null;
    }

    @Override
	public Set<ObjectChangeSet> getDeletedObjects() {
		return this.deletedObjects;
	}

	@Override
	public Set<ObjectChangeSet> getNewObjects() {
		return this.newObjectChanges;
	}

	@Override
	public boolean hasDeleted() {
		return !deletedObjects.isEmpty();
	}

	@Override
	public boolean hasChanges() {
		return hasDeleted() || hasNew() || !objectChanges.isEmpty();
	}

	@Override
	public boolean hasNew() {
		return !this.newObjectChanges.isEmpty();
	}
}

package cz.cvut.kbss.jopa.sessions;

import java.io.Serializable;
import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;

import cz.cvut.kbss.jopa.sessions.CloneBuilder;
import cz.cvut.kbss.jopa.sessions.ObjectChangeSet;
import cz.cvut.kbss.jopa.sessions.UnitOfWorkChangeSet;

public class UnitOfWorkChangeSetImpl implements Serializable, UnitOfWorkChangeSet {
	
	/**
	 * 
	 */
	private static final long serialVersionUID = 7834438138173201896L;
	//Keeps all the change sets. This is here only for future serialization
	// support
	protected Map<ObjectChangeSet, ObjectChangeSet> allchangeSets;
	protected Map<ObjectChangeSet, ObjectChangeSet> deletedObjects;
	
	protected transient Map<Class<?>, Map<ObjectChangeSet, ObjectChangeSet>> objectChanges;
	protected transient Map<Class<?>, Map<ObjectChangeSet, ObjectChangeSet>> newObjectChanges;
	
	protected boolean hasChanges;
	
	/* Reference to the UnitOfWork that is the owner of this object */
	protected transient AbstractSession session;
	
	public UnitOfWorkChangeSetImpl() {
		super();
		this.hasChanges = false;
	}

	public UnitOfWorkChangeSetImpl(AbstractSession session) {
		super();
		this.session = session;
	}

	public AbstractSession getSession() {
		return this.session;
	}

	public void addObjectChangeSet(ObjectChangeSet objectChangeSet) {
		if (objectChangeSet.isNew()) {
			this.addNewObjectChangeSet(objectChangeSet);
		}
		else {
			this.hasChanges = true;
			Map<ObjectChangeSet, ObjectChangeSet> map = getObjectChanges().get(objectChangeSet.getObjectClass());
			if (map == null) {
				map = new HashMap<ObjectChangeSet, ObjectChangeSet>();
				getObjectChanges().put(objectChangeSet.getObjectClass(), map);
			}
			map.put(objectChangeSet, objectChangeSet);
		}
		((ObjectChangeSetImpl)objectChangeSet).setUowChangeSet(this);
		getAllChangeSets().put(objectChangeSet, objectChangeSet);
	}

	public void addDeletedObjects(Map<?, ?> deletedObjects) {
		CloneBuilder builder = new CloneBuilderImpl();
		Iterator<?> it = deletedObjects.entrySet().iterator();
		while (it.hasNext()) {
			//Entries are pairs clone-original
			Map.Entry<?, ?> entry = (Entry<?, ?>) it.next();
			this.addDeletedObject(entry.getValue(), entry.getKey(), builder);
		}
	}

	//May be needed to revisit the CloneBuilder usage strategy
	public void addDeletedObject(Object deletedObject, Object clone) {
		CloneBuilder builder = new CloneBuilderImpl();
		ObjectChangeSet changeSet = builder.createObjectChangeSet(deletedObject, clone, this);
		getDeletedObjects().put(changeSet, changeSet);
	}
	
	protected void addDeletedObject(Object deletedObject, Object clone, CloneBuilder builder) {
		ObjectChangeSet changeSet = builder.createObjectChangeSet(deletedObject, clone, this);
		getDeletedObjects().put(changeSet, changeSet);
	}

	public void addNewObjectChangeSet(ObjectChangeSet newObject) {
		Map<ObjectChangeSet, ObjectChangeSet> changeSets = getNewObjectChangeSets().get(newObject.getObjectClass());
		if (changeSets == null) {
			//EL uses IdentityHashMap
			changeSets = new IdentityHashMap<ObjectChangeSet, ObjectChangeSet>();
			getNewObjectChangeSets().put(newObject.getObjectClass(), changeSets);
		}
		changeSets.put(newObject, newObject);
		((ObjectChangeSetImpl)newObject).setUowChangeSet(this);
		getAllChangeSets().put(newObject, newObject);
		this.hasChanges = true;
	}

//	public ObjectChangeSet getObjectChangeSetForClone(Object clone) {
//		// TODO Auto-generated method stub
//		return null;
//	}

	public Map<Class<?>, Map<ObjectChangeSet, ObjectChangeSet>> getObjectChanges() {
		if (this.objectChanges == null) {
			this.objectChanges = new HashMap<Class<?>, Map<ObjectChangeSet, ObjectChangeSet>>();
		}
		return this.objectChanges;
	}

	public Map<ObjectChangeSet, ObjectChangeSet> getDeletedObjects() {
		if (this.deletedObjects == null) {
			// EL uses IdentityHashMap
			this.deletedObjects = new IdentityHashMap<ObjectChangeSet, ObjectChangeSet>();
		}
		return this.deletedObjects;
	}

	public Map<Class<?>, Map<ObjectChangeSet, ObjectChangeSet>> getNewObjectChangeSets() {
		if (this.newObjectChanges == null) {
			this.newObjectChanges = new HashMap<Class<?>, Map<ObjectChangeSet, ObjectChangeSet>>();
		}
		return this.newObjectChanges;
	}
	
	public Map<ObjectChangeSet, ObjectChangeSet> getAllChangeSets() {
		if (this.allchangeSets == null) {
			// EL uses IdentityHashMap
			this.allchangeSets = new IdentityHashMap<ObjectChangeSet, ObjectChangeSet>();
		}
		return this.allchangeSets;
	}

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
		if (getAllChangeSets().size() == 0) {
			this.hasChanges = false;
		}
	}

	public boolean hasDeletedObjects() {
		return (this.deletedObjects != null) && (!this.deletedObjects.isEmpty());
	}

	public boolean hasChanges() {
		return (this.hasChanges || ((this.deletedObjects != null) && (!this.deletedObjects.isEmpty())));
	}
	
	public boolean hasNew() {
		return (this.newObjectChanges != null) && (!this.newObjectChanges.isEmpty());
	}

}

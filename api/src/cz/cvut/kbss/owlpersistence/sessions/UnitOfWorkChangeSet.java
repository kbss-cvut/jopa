package cz.cvut.kbss.owlpersistence.sessions;

import java.util.Map;

public interface UnitOfWorkChangeSet {
	/**
	 * Add new ObjectChangeSet to this changeSet.
	 * @param objectChangeSet ObjectChangeSet
	 */
	public void addObjectChangeSet(ObjectChangeSet objectChangeSet);
	
	/**
	 * Add objects which are to be deleted.
	 * @param deletedObjects Map
	 */
	public void addDeletedObjects(Map<?, ?> deletedObjects);
	
	/**
	 * Add an object which is to be deleted.
	 * @param deletedObject Object
	 */
	public void addDeletedObject(Object deletedObject, Object clone);
	
	/**
	 * Add a change set for newly created object. These changes are
	 * held in separate attribute and get special treatment when merged
	 * into shared session cache.
	 * @param newObject ObjectChangeSet
	 */
	public void addNewObjectChangeSet(ObjectChangeSet newObject);
	
//	/**
//	 * This method returns the ObjectChangeSet for the given clone.
//	 * @param clone Object
//	 * @return ObjectChangeSet
//	 */
//	public ObjectChangeSet getObjectChangeSetForClone(Object clone);
	
	/**
	 * Returns the collection of all ObjectChangeSets excluding change sets
	 * for new objects and deleted objects.
	 * @return java.util.Map
	 */
	public Map<?, ?> getObjectChanges();
	
	/**
	 * Returns the collection of deleted objects.
	 * @return java.util.Map
	 */
	public Map<?, ?> getDeletedObjects();
	
	/**
	 * Returns the collection of change set for newly created objects.
	 * @return java.util.Map
	 */
	public Map<?, ?> getNewObjectChangeSets();
	
	/**
	 * Remove the specified object change set.
	 * @param changeSet ObjectChangeSet
	 */
	public void removeObjectChangeSet(ObjectChangeSet changeSet);
	
	/**
	 * Returns true if there are deleted objects in this change set.
	 * @return boolean
	 */
	public boolean hasDeletedObjects();
	
	/**
	 * Returns true if this changeSet has any changes.
	 * @return boolean
	 */
	public boolean hasChanges();
	
	/**
	 * Are there any new objects in the change set?
	 * @return boolean
	 */
	public boolean hasNew();
}

package cz.cvut.kbss.jopa.sessions;

import java.util.Map;

public interface UnitOfWorkChangeSet {
	/**
	 * Add new ObjectChangeSet to this changeSet.
	 * 
	 * @param objectChangeSet
	 *            ObjectChangeSet
	 */
	public void addObjectChangeSet(ObjectChangeSet objectChangeSet);

	/**
	 * Add a change set for newly created object. These changes are held in
	 * separate attribute and get special treatment when merged into shared
	 * session cache.
	 * 
	 * @param newObject
	 *            ObjectChangeSet
	 */
	public void addNewObjectChangeSet(ObjectChangeSet newObject);

	/**
	 * Adds a change set for deleted object.
	 * 
	 * @param deletedObject
	 *            The change set to add
	 */
	public void addDeletedObject(ObjectChangeSet deletedObject);

	/**
	 * Returns the collection of all ObjectChangeSets excluding change sets for
	 * new objects and deleted objects.
	 * 
	 * @return java.util.Map
	 */
	public Map<?, ?> getObjectChanges();

	/**
	 * Returns the collection of deleted objects.
	 * 
	 * @return java.util.Map
	 */
	public Map<?, ?> getDeletedObjects();

	/**
	 * Returns the collection of change set for newly created objects.
	 * 
	 * @return java.util.Map
	 */
	public Map<?, ?> getNewObjectChangeSets();

	/**
	 * Remove the specified object change set.
	 * 
	 * @param changeSet
	 *            ObjectChangeSet
	 */
	public void removeObjectChangeSet(ObjectChangeSet changeSet);

	/**
	 * Returns true if there are deleted objects in this change set.
	 * 
	 * @return boolean
	 */
	public boolean hasDeletedObjects();

	/**
	 * Returns true if this changeSet has any changes.
	 * 
	 * @return boolean
	 */
	public boolean hasChanges();

	/**
	 * Are there any new objects in the change set?
	 * 
	 * @return boolean
	 */
	public boolean hasNew();
}

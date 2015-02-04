package cz.cvut.kbss.jopa.sessions;

import java.util.Collection;
import java.util.Set;

public interface UnitOfWorkChangeSet {
    /**
     * Add new ObjectChangeSet to this changeSet.
     *
     * @param objectChangeSet ObjectChangeSet
     */
    public void addObjectChangeSet(ObjectChangeSet objectChangeSet);

    /**
     * Add a change set for newly created object. These changes are held in
     * separate attribute and get special treatment when merged into shared
     * session cache.
     *
     * @param newObject ObjectChangeSet
     */
    public void addNewObjectChangeSet(ObjectChangeSet newObject);

    /**
     * Adds a change set for deleted object.
     *
     * @param deletedObject The change set to add
     */
    public void addDeletedObjectChangeSet(ObjectChangeSet deletedObject);

    /**
     * Returns change sets for existing modified objects.
     * <p/>
     * New object and deleted object change sets are not included.
     *
     * @return Collection of change sets
     */
    public Collection<ObjectChangeSet> getExistingObjectsChanges();

    /**
     * Gets changes for the specified original object (if there are any).
     *
     * @param original The object for which changes should be found
     * @return Object change set or null, if the object has no changes
     */
    public ObjectChangeSet getExistingObjectChanges(Object original);

    /**
     * Returns the collection of deleted objects.
     *
     * @return Set of change sets
     */
    public Set<ObjectChangeSet> getDeletedObjects();

    /**
     * Returns the collection of change sets for newly created objects.
     *
     * @return Set of change sets
     */
    public Set<ObjectChangeSet> getNewObjects();

    /**
     * Returns true if there are deleted objects in this change set.
     *
     * @return boolean
     */
    public boolean hasDeleted();

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

package cz.cvut.kbss.owlpersistence.sessions;

/**
 * This interface defines the way changes are merged into session cache.
 * 
 * @author kidney
 *
 */
public interface MergeManager {
	
	/**
	 * Merge changes from one ObjectChangeSet, which represents the changes
	 * made to clone, into the original object.
	 * @param clone Object
	 * @param changeSet ObjectChangeSet
	 * @return Object The merged object.
	 */
	public Object mergeChangesOnObject(Object clone, ObjectChangeSet changeSet);
	
	/**
	 * Merge changes from the provided UnitOfWorkChangeSet into the target session.
	 * @param changeSet UnitOfWorkChangeSet
	 */
	public void mergeChangesFromChangeSet(UnitOfWorkChangeSet changeSet);
	
	/**
	 * Merge newly created object into the shared session cache.
	 * @param changeSet ObjectChangeSet
	 */
	public void mergeNewObject( ObjectChangeSet changeSet);

}

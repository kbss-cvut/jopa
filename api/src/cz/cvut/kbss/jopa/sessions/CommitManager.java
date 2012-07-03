package cz.cvut.kbss.jopa.sessions;

public interface CommitManager {

	/**
	 * Add the new entities specified by the given changeSet to the ontology.
	 * 
	 * @param changeSet
	 *            UnitOfWorkChangeSet
	 */
	public void commitNewEntities(UnitOfWorkChangeSet changeSet);

	/**
	 * Commit all the changes from the changeSet to the ontology. This involves
	 * adding new entities and deleting entities to be deleted, so the client
	 * does not have to use special operations (but he can).
	 * 
	 * @param changeSet
	 *            UnitOfWorkChangeSet
	 */
	public void commitChanges(UnitOfWorkChangeSet changeSet);

	/**
	 * Delete the entities specified by the changeSet from the ontology.
	 * 
	 * @param changeSet
	 *            UnitOfWorkChangeSet
	 */
	public void commitDeleted(UnitOfWorkChangeSet changeSet);

}

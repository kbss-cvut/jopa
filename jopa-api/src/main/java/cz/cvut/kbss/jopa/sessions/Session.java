package cz.cvut.kbss.jopa.sessions;

import java.util.List;

import cz.cvut.kbss.jopa.model.Repository;

public interface Session {

	/**
	 * Acquires UnitOfWork object to perform transaction operations.
	 * 
	 * @return UnitOfWork
	 */
	public UnitOfWork acquireUnitOfWork();

	/**
	 * Release this session and all its children.
	 */
	public void release();

	/**
	 * Remove the given object from the session's live object cache. This is
	 * particularly meant for merging deleted objects from transactions.
	 * 
	 * @param object
	 *            Object
	 * @param entityOrigin
	 *            Entity origin identifier
	 */
	public void removeObjectFromCache(Object object, EntityOrigin entityOrigin);

	/**
	 * Gets repositories available to this session. </p>
	 * 
	 * The repositories are ordered by their priority.
	 * 
	 * @return Unmodifiable list of repositories
	 */
	public List<Repository> getRepositories();
}

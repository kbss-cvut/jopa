package cz.cvut.kbss.jopa.sessions;

import java.net.URI;

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
	 * @param context
	 *            Entity context URI
	 */
	public void removeObjectFromCache(Object object, URI context);
}

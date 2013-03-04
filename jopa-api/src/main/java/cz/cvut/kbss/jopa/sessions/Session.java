package cz.cvut.kbss.jopa.sessions;

import java.util.List;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.model.query.Query;
import cz.cvut.kbss.jopa.model.query.TypedQuery;

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
	 * Create an instance of Query for executing a Java Persistence query
	 * language statement.
	 * 
	 * @param qlString
	 *            a Java Persistence query string
	 * @param em
	 *            The calling EntityManager instance.
	 * @return the new query instance
	 * @throws IllegalArgumentException
	 *             if query string is not valid
	 */
	public Query<?> createQuery(String qlString, final EntityManager em);

	/**
	 * Create an instance of Query for executing typed Java Persistence query
	 * language statement.
	 * 
	 * @param query
	 *            A Java Persistence query.
	 * @param resultClass
	 *            Result type.
	 * @param sparql
	 *            True if the specified query is a Sparql query.
	 * @param em
	 *            The calling EntityManager instance.
	 * @return The new query instance.
	 */
	public <T> TypedQuery<T> createQuery(String query, Class<T> resultClass,
			boolean sparql, final EntityManager em);

	/**
	 * Create an instance of Query for executing a native SPARQL-DL query in
	 * SPARQL syntax.
	 * 
	 * @param sparql
	 *            a native SQL query string
	 * @param em
	 *            The calling EntityManager instance.
	 * @return the new query instance
	 */
	public Query<List<String>> createNativeQuery(String sparql,
			final EntityManager em);

	/**
	 * Remove the given object from the session's live object cache. This is
	 * particularly meant for merging deleted objects from transactions.
	 * 
	 * @param object
	 *            Object
	 */
	public void removeObjectFromCache(Object object);
}

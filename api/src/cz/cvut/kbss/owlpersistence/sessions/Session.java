package cz.cvut.kbss.owlpersistence.sessions;

import java.util.Vector;

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
	 * Execute the given SPARQL 1.1 query.
	 * 
	 * @param query
	 *            Query
	 * @return Vector Returns a Vector of records.
	 */
	public Vector<?> executeQuery(String sparqlQuery);

	/**
	 * Reads all objects of the specified class from the ontology.
	 * 
	 * @param domainClass
	 *            Class
	 * @return Vector Returns a Vector of objects of the given class.
	 */
	public Vector<?> readAllObjects(Class<?> domainClass);

	/**
	 * Reads the first object of the given Class from the ontology.
	 * 
	 * @param domainClass
	 *            Class
	 * @return Object
	 */
	public Object readObject(Class<?> domainClass);

	/**
	 * Reads the first object with the same primary key from the ontology.
	 * 
	 * @param cls
	 *            Class
	 * @param primaryKey
	 *            Object
	 * @return
	 */
	public <T> T readObject(Class<T> cls, Object primaryKey);

	/**
	 * Remove the given object from the session's live object cache. This is
	 * particularly meant for merging deleted objects from transactions.
	 * 
	 * @param object
	 *            Object
	 */
	public void removeObjectFromCache(Object object);
}

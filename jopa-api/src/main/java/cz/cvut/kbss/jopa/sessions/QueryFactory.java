package cz.cvut.kbss.jopa.sessions;

import java.util.List;

import cz.cvut.kbss.jopa.model.RepositoryID;
import cz.cvut.kbss.jopa.model.query.Query;
import cz.cvut.kbss.jopa.model.query.TypedQuery;

interface QueryFactory {

	/**
	 * Creates query object representing a native SPARQL query. </p>
	 * 
	 * @param sparql
	 *            The query
	 * @param repository
	 *            Identifier of repository context(s) against which the query
	 *            will be evaluated
	 * @return Query object
	 * @throws NullPointerException
	 *             If {@code sparql} is {@code null}
	 */
	public Query<List<String>> createNativeQuery(String sparql, RepositoryID repository);

	/**
	 * Creates typed query object representing a native SPARQL query. </p>
	 * 
	 * @param sparql
	 *            The query
	 * @param resultClass
	 *            Type of the results
	 * @param repository
	 *            Identifier of repository context(s) against which the query
	 *            will be evaluated
	 * @return Query object
	 * @throws NullPointerException
	 *             If {@code sparql} or {@code resultClass} is {@code null}
	 */
	public <T> TypedQuery<T> createNativeQuery(String sparql, Class<T> resultClass,
			RepositoryID repository);

	/**
	 * Creates query object representing a native SPARQL query. </p>
	 * 
	 * @param sparql
	 *            The query
	 * @param repository
	 *            Identifier of repository context(s) against which the query
	 *            will be evaluated
	 * @return Query object
	 * @throws NullPointerException
	 *             If {@code sparql} is {@code null}
	 */
	public Query createQuery(String query, RepositoryID repository);

	/**
	 * Creates typed query object representing a native SPARQL query. </p>
	 * 
	 * @param sparql
	 *            The query
	 * @param resultClass
	 *            Type of the results param URI of the ontology context against
	 *            which the query will be evaluated
	 * @param repository
	 *            Identifier of repository context(s) against which the query
	 *            will be evaluated
	 * @return Query object
	 * @throws NullPointerException
	 *             If {@code sparql} or {@code resultClass} is {@code null}
	 */
	public <T> TypedQuery<T> createQuery(String query, Class<T> resultClass, RepositoryID repository);
}

package cz.cvut.kbss.jopa.sessions;

import java.util.List;

import cz.cvut.kbss.jopa.model.query.Query;
import cz.cvut.kbss.jopa.model.query.TypedQuery;

public interface QueryFactory {

	/**
	 * Creates query object representing a native SPARQL query. </p>
	 * 
	 * @param sparql
	 *            The query
	 * @return Query object
	 * @throws NullPointerException
	 *             If {@code sparql} is {@code null}
	 */
	Query<List<String>> createNativeQuery(String sparql);

	/**
	 * Creates typed query object representing a native SPARQL query. </p>
	 * 
	 * @param sparql
	 *            The query
	 * @param resultClass
	 *            Type of the results
	 * @return Query object
	 * @throws NullPointerException
	 *             If {@code sparql} or {@code resultClass} is {@code null}
	 */
	<T> TypedQuery<T> createNativeQuery(String sparql, Class<T> resultClass);

	/**
	 * Creates query object representing a native SPARQL query. </p>
	 * 
	 * @param query
	 *            The query
	 * @return Query object
	 * @throws NullPointerException
	 *             If {@code sparql} is {@code null}
	 */
	Query createQuery(String query);

	/**
	 * Creates typed query object representing a native SPARQL query. </p>
	 * 
	 * @param query
	 *            The query
	 * @param resultClass
	 *            Type of the results param URI of the ontology context against
	 *            which the query will be evaluated
	 * @return Query object
	 * @throws NullPointerException
	 *             If {@code sparql} or {@code resultClass} is {@code null}
	 */
	<T> TypedQuery<T> createQuery(String query, Class<T> resultClass);
}

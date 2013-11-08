package cz.cvut.kbss.jopa.sessions;

import java.net.URI;
import java.util.List;

import cz.cvut.kbss.jopa.model.query.Query;
import cz.cvut.kbss.jopa.model.query.TypedQuery;

interface QueryFactory {

	/**
	 * Creates query object representing a native SPARQL query. </p>
	 * 
	 * @param sparql
	 *            The query
	 * @param contextUri
	 *            URI of the ontology context against which the query will be
	 *            evaluated
	 * @return Query object
	 * @throws NullPointerException
	 *             If {@code sparql} is {@code null}
	 */
	public Query<List<String>> createNativeQuery(String sparql, URI contextUri);

	/**
	 * Creates typed query object representing a native SPARQL query. </p>
	 * 
	 * @param sparql
	 *            The query
	 * @param resultClass
	 *            Type of the results
	 * @param contextUri
	 *            URI of the ontology context against which the query will be
	 *            evaluated
	 * @return Query object
	 * @throws NullPointerException
	 *             If {@code sparql} or {@code resultClass} is {@code null}
	 */
	public <T> TypedQuery<T> createNativeQuery(String sparql, Class<T> resultClass, URI contextUri);

	/**
	 * Creates query object representing a native SPARQL query. </p>
	 * 
	 * @param sparql
	 *            The query
	 * @param contextUri
	 *            URI of the ontology context against which the query will be
	 *            evaluated
	 * @return Query object
	 * @throws NullPointerException
	 *             If {@code sparql} is {@code null}
	 */
	public Query createQuery(String query, URI contextUri);

	/**
	 * Creates typed query object representing a native SPARQL query. </p>
	 * 
	 * @param sparql
	 *            The query
	 * @param resultClass
	 *            Type of the results param URI of the ontology context against
	 *            which the query will be evaluated
	 * @return Query object
	 * @throws NullPointerException
	 *             If {@code sparql} or {@code resultClass} is {@code null}
	 */
	public <T> TypedQuery<T> createQuery(String query, Class<T> resultClass, URI contextUri);
}

package cz.cvut.kbss.ontodriver.impl.sesame;

import java.util.Collection;

import org.openrdf.model.Model;
import org.openrdf.model.Resource;
import org.openrdf.model.Statement;
import org.openrdf.model.URI;
import org.openrdf.model.Value;
import org.openrdf.query.TupleQueryResult;

import cz.cvut.kbss.ontodriver.Closeable;
import cz.cvut.kbss.ontodriver.exceptions.QueryExecutionException;

/**
 * Proxy to the Sesame storage. </p>
 * 
 * The implementation decides whether to cache all the stuff or use a live
 * connection to the storage.
 * 
 * @author ledvima1
 * 
 */
interface StorageProxy extends Closeable {

	/**
	 * Filter repository according to the subject, predicate and object.
	 * 
	 * @param subject
	 *            Subject
	 * @param predicate
	 *            Property
	 * @param object
	 *            Object
	 * @param includeInferred
	 *            Whether inferred statements should be included
	 * @return A model containing the resulting statements
	 */
	public Model filter(Resource subject, URI predicate, Value object, boolean includeInferred);

	/**
	 * Adds statements to the storage.
	 * 
	 * @param statements
	 *            Statements to add
	 */
	public void addStatements(Collection<Statement> statements);

	/**
	 * Adds statement to the storage.
	 * 
	 * @param statement
	 *            The statement to add
	 */
	public void addStatement(Statement statement);

	/**
	 * Removes statements from the storage.
	 * 
	 * @param statements
	 *            Statements to remove
	 */
	public void removeStatements(Collection<Statement> statements);

	/**
	 * Whether the storage contains an individual with the specified URI. </p>
	 * 
	 * Note that only subjects and objects are checked, it is not expected to
	 * treat the specified URI as predicate.
	 * 
	 * @param uri
	 *            Individual URI
	 * @return true if contains, false otherwise
	 */
	public boolean contains(URI uri);

	/**
	 * Executes the specified SPARQL query.
	 * 
	 * @param query
	 *            The query to execute
	 * @return Query result set
	 */
	public TupleQueryResult executeQuery(String query) throws QueryExecutionException;
}

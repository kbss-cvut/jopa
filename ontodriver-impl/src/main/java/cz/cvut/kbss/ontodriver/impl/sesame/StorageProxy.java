package cz.cvut.kbss.ontodriver.impl.sesame;

import java.util.Collection;
import java.util.Set;

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
	 * @param context
	 *            Context to which the search shall be restricted. If it is
	 *            {@code null}, the whole repository is searched
	 * @return A model containing the resulting statements
	 */
	public Model filter(Resource subject, URI predicate, Value object, boolean includeInferred,
			URI context);

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
	 * @param contexts
	 *            Contexts to which the search shall be restricted. If the
	 *            collection is empty, the search is performed on the whole
	 *            repository
	 * @return A model containing the resulting statements
	 */
	public Model filter(Resource subject, URI predicate, Value object, boolean includeInferred,
			Collection<URI> contexts);

	/**
	 * Adds statements to the storage.
	 * 
	 * @param statements
	 *            Statements to add
	 * @param context
	 *            Context into which the statements will be added
	 */
	public void addStatements(Collection<Statement> statements, URI context);

	/**
	 * Adds statement to the storage.
	 * 
	 * @param statement
	 *            The statement to add
	 * @param context
	 *            Context into which the statement will be added
	 */
	public void addStatement(Statement statement, URI context);

	/**
	 * Removes the specified statement from the specified context.
	 * 
	 * @param statement
	 *            The statement to remove
	 * @param context
	 *            Context from which the statement should be removed. Can be
	 *            {@code null}
	 */
	public void removeStatement(Statement statement, URI context);

	/**
	 * Removes statements from the storage.
	 * 
	 * @param statements
	 *            Statements to remove
	 * @param context
	 *            Context from which the statements will be removed
	 */
	public void removeStatements(Collection<Statement> statements, URI context);

	/**
	 * Whether the storage contains an individual with the specified URI. </p>
	 * 
	 * Note that only subjects and objects are checked, it is not expected to
	 * treat the specified URI as predicate.
	 * 
	 * @param uri
	 *            Individual URI
	 * @param contexts
	 *            Set of context URIs to search
	 * @return true if contains, false otherwise
	 */
	public boolean contains(URI uri, Set<URI> contexts);

	/**
	 * Whether the data in the storage assert that the specified subject is an
	 * instance of {@code type}. </p>
	 * 
	 * This method searches in both explicit and inferred statements.
	 * 
	 * @param subject
	 *            Subject URI
	 * @param type
	 *            Class URI
	 * @return True if storage asserts the type of the subject to be
	 *         {@code type}, false otherwise
	 */
	public boolean isSubjectOfType(URI subject, URI type);

	/**
	 * Executes the specified SPARQL query.
	 * 
	 * @param query
	 *            The query to execute
	 * @return Query result set
	 */
	public TupleQueryResult executeQuery(String query) throws QueryExecutionException;
}

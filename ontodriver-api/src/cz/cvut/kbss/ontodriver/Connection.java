package cz.cvut.kbss.ontodriver;

import java.net.URI;
import java.util.List;

/**
 * Represents a connection to the underlying OntoDriver. </p>
 * 
 * A single OntoDriver can manage multiple storages at once. Each of the storage
 * can even be of a different type (OWL, RDF) and different profile.
 * 
 * @author kidney
 * 
 */
public interface Connection {

	/**
	 * Closes this connection. </p>
	 * 
	 * Closing an already closed connection does nothing. If there is a
	 * transaction running when {@code close} is called, the transaction is
	 * rolled back. However, it is strongly recommended to commit or roll back
	 * transaction explicitly.
	 * 
	 * @throws OntoDriverException
	 *             If an ontology access error occurs
	 */
	public void close() throws OntoDriverException;

	/**
	 * Commits the current ontology transaction making all pending changes
	 * persistent. </p>
	 * 
	 * This method should not be called when in auto-commit mode.
	 * 
	 * @throws OntoDriverException
	 *             If in auto-commit mode, called on a closed connection or an
	 *             ontology access error occurs
	 */
	public void commit() throws OntoDriverException;

	/**
	 * Creates a new SPARQL statement.
	 * 
	 * @return a {@code Statement} instance
	 * @throws OntoDriverException
	 *             If called on a closed connection or an ontology access error
	 *             occurs
	 */
	public Statement createStatement() throws OntoDriverException;

	/**
	 * Retrieves a list of all available contexts. </p>
	 * 
	 * A context in this scenario can be a named graph, an ontology or an
	 * ontology module.
	 * 
	 * @return List of context URIs
	 * @throws OntoDriverException
	 *             If called on a closed connection or an ontology access error
	 *             occurs
	 */
	public List<URI> getContexts() throws OntoDriverException;

	/**
	 * Retrieves saving context for the specified entity. </p>
	 * 
	 * If {@code entity} was loaded from an ontology, its loading context is
	 * also its saving context. If {@code entity} is to be persisted, the
	 * default saving context is returned.
	 * 
	 * @param primaryKey
	 *            URI of the entity to look context up for
	 * @return Context URI
	 * @throws OntoDriverException
	 *             If called on a closed connection or an ontology access error
	 *             occurs
	 * @see #setDefaultContext(String)
	 */
	public URI getSaveContextFor(Object primaryKey) throws OntoDriverException;

	/**
	 * Creates and returns a new prepared SPARQL statement. </p>
	 * 
	 * @param sparql
	 *            The query to prepare
	 * @return {@code PreparedStatement}
	 * @throws OntoDriverException
	 *             If called on a closed connection or an ontology access error
	 *             occurs
	 */
	public PreparedStatement prepareStatement(String sparql)
			throws OntoDriverException;

	/**
	 * Rolls back the current transaction undoing any pending changes. </p>
	 * 
	 * This method should not be called when in auto-commit mode.
	 * 
	 * @throws OntoDriverException
	 *             If called on a closed connection or an ontology access error
	 *             occurs
	 */
	public void rollback() throws OntoDriverException;

	/**
	 * Sets auto commit mode on this connection. </p>
	 * 
	 * Setting auto commit twice on to the same value has no effect.
	 * 
	 * @param autoCommit
	 *            True if setting to auto-commit mode, false otherwise
	 * @throws OntoDriverException
	 *             If called on a closed connection or an ontology access error
	 *             occurs
	 */
	public void setAutoCommit(boolean autoCommit) throws OntoDriverException;

	/**
	 * Sets default saving context for this connection. </p>
	 * 
	 * This context is used for newly persisted entities without saving context.
	 * 
	 * @param context
	 *            The context to use as default for persist
	 * @throws OntoDriverException
	 *             If the context is not valid, called on a closed connection or
	 *             an ontology access error occurs
	 */
	public void setDefaultContext(URI context) throws OntoDriverException;

	/**
	 * Sets saving context for entity with the specified primary key. </p>
	 * 
	 * @param primaryKey
	 *            Primary key of the entity to set context for
	 * @param context
	 *            The context URi
	 * @throws OntoDriverException
	 *             If called on a closed connection, the context is not valid or
	 *             an ontology access error occurs
	 */
	// TODO What about auto-generated primary keys
	public void setSaveContextFor(Object primaryKey, URI context)
			throws OntoDriverException;

	// TODO find, merge, persist, remove - how should they be typed? Sending the
	// whole entity would require OntoDriver to have access to metamodel,
	// sending only OWLIndividual/Resource would require additional methods for
	// working with attributes of the entity
}

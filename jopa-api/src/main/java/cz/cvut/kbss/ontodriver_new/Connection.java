package cz.cvut.kbss.ontodriver_new;

import java.net.URI;
import java.util.Collection;
import java.util.Set;

import cz.cvut.kbss.ontodriver.PreparedStatement;
import cz.cvut.kbss.ontodriver.Statement;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver_new.model.Axiom;
import cz.cvut.kbss.ontodriver_new.model.NamedResource;

public interface Connection extends AutoCloseable {
	// TODO

	/**
	 * Whether this connection is still active. </p>
	 */
	public boolean isOpen();

	/**
	 * Commits this connection. </p>
	 * 
	 * This effectively makes persistent any changes made since the last
	 * commit/rollback or since this connection was opened.
	 * 
	 * @throws OntoDriverException
	 *             If an ontology access error occurs
	 * @throws IllegalStateException
	 *             If called on a closed connection
	 */
	public void commit() throws OntoDriverException;

	/**
	 * Rolls back any changes made in the current transaction.
	 * 
	 * @throws OntoDriverException
	 *             If an ontology access error occurs
	 * @throws IllegalStateException
	 *             If called on a closed connection
	 */
	public void rollback() throws OntoDriverException;

	/**
	 * Sets this connection's auto-commit mode to the specified state.
	 * 
	 * @param autoCommit
	 *            The new auto-commit state
	 * @throws IllegalStateException
	 *             If called on a closed connection
	 */
	public void setAutoCommit(boolean autoCommit);

	/**
	 * Returns this connection's auto-commit mode.
	 * 
	 * @return {@code true} if this connection is in auto-commit mode,
	 *         {@code false} otherwise
	 * @throws IllegalStateException
	 *             If called on a closed connection
	 */
	public boolean isAutoCommit();

	/**
	 * Creates a new SPARQL statement.
	 * 
	 * @return a {@code Statement} instance
	 * @throws OntoDriverException
	 *             If an ontology access error occurs
	 * @throws IllegalStateException
	 *             If called on a closed connection
	 */
	public Statement createStatement() throws OntoDriverException;

	/**
	 * Creates and returns a new prepared SPARQL statement. </p>
	 * 
	 * @param sparql
	 *            The query to prepare
	 * @return {@code PreparedStatement}
	 * @throws OntoDriverException
	 *             If an ontology access error occurs
	 * @throws IllegalStateException
	 *             If called on a closed connection
	 */
	public PreparedStatement prepareStatement(String sparql) throws OntoDriverException;

	/**
	 * Verifies consistency of ontology context with the specified URI. </p>
	 * 
	 * Note that {@code null} argument means checking consistency of the whole
	 * repository.
	 * 
	 * @param context
	 *            Context identifier, can be {@code null}
	 * @return Consistency status
	 * @throws OntoDriverException
	 *             If an ontology access error occurs
	 * @throws IllegalStateException
	 *             If called on a closed connection
	 */
	public boolean isConsistent(URI context) throws OntoDriverException;

	/**
	 * Gets a set of currently available contexts in the underlying repository.
	 * </p>
	 * 
	 * Note that the default context is not included in the result.
	 * 
	 * @return Set of context URIs
	 * @throws OntoDriverException
	 *             If an ontology access error occurs
	 * @throws IllegalStateException
	 *             If called on a closed connection
	 */
	public Set<URI> getContexts() throws OntoDriverException;

	/**
	 * Finds axioms with the corresponding subject and properties. </p>
	 * 
	 * @param descriptor
	 *            Loading descriptor specifies subject, properties to load and
	 *            possible contexts to work with
	 * @return Collection of axioms matching the specified criteria
	 * @throws OntoDriverException
	 *             If an ontology access error occurs
	 * @throws IllegalStateException
	 *             If called on a closed connection
	 */
	public Collection<Axiom> find(AxiomDescriptor descriptor) throws OntoDriverException;

	/**
	 * Gets types of the specified subject. </p>
	 * 
	 * The types are returned as a collection of class assertion axioms with the
	 * specified subjects.
	 * 
	 * @param subject
	 *            The subject to find types for
	 * @param context
	 *            In which context to search. Can be {@code null}
	 * @param includeInferred
	 *            Whether inferred types should be included as well
	 * @return Collection of class assertion axioms
	 * @throws OntoDriverException
	 *             If an ontology access error occurs
	 * @throws IllegalStateException
	 *             If called on a closed connection
	 */
	public Collection<Axiom> getTypes(NamedResource subject, URI context, boolean includeInferred);

	// persist
	// update
	// remove
}

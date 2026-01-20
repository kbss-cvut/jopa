/*
 * JOPA
 * Copyright (C) 2025 Czech Technical University in Prague
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */
package cz.cvut.kbss.ontodriver;

import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.AxiomValueDescriptor;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;

import java.net.URI;
import java.util.Collection;
import java.util.List;
import java.util.Set;

public interface Connection extends AutoCloseable, Wrapper {

    /**
     * Whether this connection is active.
     *
     * @return Active status of this connection
     */
    boolean isOpen();

    /**
     * Commits this connection.
     * <p>
     * This effectively makes persistent any changes made since the last commit/rollback, or since this connection was
     * opened.
     * <p>
     * If this connection is in auto-commit mode, calling this method has no effect.
     *
     * @throws OntoDriverException   If an ontology access error occurs
     * @throws IllegalStateException If called on a closed connection
     */
    void commit() throws OntoDriverException;

    /**
     * Rolls back any changes made in the current transaction.
     * <p>
     * If this connection is in auto-commit mode, calling this method has no effect.
     *
     * @throws OntoDriverException   If an ontology access error occurs
     * @throws IllegalStateException If called on a closed connection
     */
    void rollback() throws OntoDriverException;

    /**
     * Sets this connection's auto-commit mode to the specified state.
     *
     * @param autoCommit The new auto-commit state
     * @throws IllegalStateException If called on a closed connection
     */
    void setAutoCommit(boolean autoCommit);

    /**
     * Returns this connection's auto-commit mode.
     *
     * @return {@code true} if this connection is in auto-commit mode, {@code false} otherwise
     * @throws IllegalStateException If called on a closed connection
     */
    boolean isAutoCommit();

    /**
     * Puts this connection in read-only mode as a hint to the driver to enable database optimizations.
     * <p>
     * <b>Note</b>: This method cannot be called during a transaction.
     *
     * @param readOnly {@code true} enables read-only mode, {@code false} disables it
     */
    void setReadOnly(boolean readOnly);

    /**
     * Retrieves whether this Connection object is in read-only mode.
     *
     * @return {@code true} if this Connection object is read-only; {@code false} otherwise
     * @throws IllegalStateException If called on a closed connection
     */
    boolean isReadOnly();

    /**
     * Creates a new SPARQL statement.
     *
     * @return a {@code Statement} instance
     * @throws OntoDriverException   If an ontology access error occurs
     * @throws IllegalStateException If called on a closed connection
     */
    Statement createStatement() throws OntoDriverException;

    /**
     * Creates and returns a new prepared SPARQL statement.
     *
     * @param sparql The query to prepare
     * @return {@code PreparedStatement}
     * @throws OntoDriverException   If an ontology access error occurs
     * @throws IllegalStateException If called on a closed connection
     */
    PreparedStatement prepareStatement(String sparql) throws OntoDriverException;

    /**
     * Verifies consistency of ontology context with the specified URI
     * <p>
     * Note that {@code null} argument means checking consistency of the whole repository.
     *
     * @param context Context identifier, can be {@code null}
     * @return Consistency status
     * @throws OntoDriverException   If an ontology access error occurs
     * @throws IllegalStateException If called on a closed connection
     */
    boolean isConsistent(URI context) throws OntoDriverException;

    /**
     * Gets a set of currently available contexts in the underlying repository.
     * <p>
     * Note that the default context is not included in the result.
     *
     * @return List of context URIs
     * @throws OntoDriverException   If an ontology access error occurs
     * @throws IllegalStateException If called on a closed connection
     */
    List<URI> getContexts() throws OntoDriverException;

    /**
     * Checks whether the storage contains the specified axiom.
     * <p>
     * The contexts optionally specify repository contexts in which to look for the axiom.
     *
     * @param axiom    The axiom to look for
     * @param contexts Optional search contexts, an empty set means to look in the default storage context
     * @return {@code true} if the storage contains matching axiom, {@code false} otherwise
     * @throws OntoDriverException   If an ontology access error occurs
     * @throws IllegalStateException If called on a closed connection
     */
    boolean contains(Axiom<?> axiom, Set<URI> contexts) throws OntoDriverException;

    /**
     * Checks whether the specified axiom is inferred in the underlying repository.
     * <p>
     * The contexts optionally specify repository contexts in which to look for the axiom. However, note that different
     * triple stores treat inference differently and inferred statements may be stored in a special context. In this
     * case, the implementation may choose to ignore the provided set of contexts.
     * <p>
     * Also note that since most underlying repository access implementations do not allow retrieving inferred
     * statements only and instead provide either only asserted or asserted + inferred statements, this method may
     * return {@code false} in case the repository contains the axiom both inferred and asserted.
     *
     * @param axiom    Axiom whose inference to check
     * @param contexts Optional search contexts, an empty set means to look in the default storage context
     * @return {@code true} if the repository inferred the specified axiom, {@code false} if the axiom is asserted or
     * not present at all
     * @throws OntoDriverException   If an ontology access error occurs
     * @throws IllegalStateException If called on a closed connection
     */
    boolean isInferred(Axiom<?> axiom, Set<URI> contexts) throws OntoDriverException;

    /**
     * Finds axioms with the corresponding subject and properties.
     *
     * @param descriptor Loading descriptor specifies subject, properties to load and possible contexts to work with
     * @return Collection of axioms matching the specified criteria
     * @throws OntoDriverException   If an ontology access error occurs
     * @throws IllegalStateException If called on a closed connection
     */
    Collection<Axiom<?>> find(AxiomDescriptor descriptor) throws OntoDriverException;

    /**
     * Persists new individual and its property values specified by the descriptor.
     *
     * @param descriptor Descriptor of the persisted values
     * @throws OntoDriverException   If an ontology access error occurs
     * @throws IllegalStateException If called on a closed connection
     */
    void persist(AxiomValueDescriptor descriptor) throws OntoDriverException;

    /**
     * Generates a new unique identifier based on the specified type.
     * <p>
     * The identifier is required to be unique in the whole repository.
     *
     * @param classUri OWL class identifier
     * @return Unique identifier
     * @throws OntoDriverException If an ontology access error occurs
     */
    URI generateIdentifier(URI classUri) throws OntoDriverException;

    /**
     * Persists the values specified by this descriptor, removing existing property values from the ontology.
     * <p>
     * This method removes original values of properties specified in the descriptor and persists new values specified
     * therein.
     *
     * @param descriptor Descriptor of the update values
     * @throws OntoDriverException   If an ontology access error occurs
     * @throws IllegalStateException If called on a closed connection
     */
    void update(AxiomValueDescriptor descriptor) throws OntoDriverException;

    /**
     * Removes all axioms related to subject specified by the descriptor.
     * <p>
     * The descriptor may also specify contexts from which property assertion axioms should be removed.
     * <p>
     * Note that this method will cause also removal of axioms in which the {@link NamedResource} specified by the
     * argument stands as value.
     *
     * @param descriptor Descriptor of contexts and the subject of removal
     * @throws OntoDriverException   If an ontology access error occurs
     * @throws IllegalStateException If called on a closed connection
     */
    void remove(AxiomDescriptor descriptor) throws OntoDriverException;

    /**
     * Gets ontology lists handler.
     *
     * @return Lists handler
     */
    Lists lists();

    /**
     * Gets types handler.
     *
     * @return Types handler
     */
    Types types();

    /**
     * Gets handler for unmapped properties.
     *
     * @return Properties handler
     */
    Properties properties();

    /**
     * Gets handler for RDF containers.
     *
     * @return Containers handler
     */
    Containers containers();

    /**
     * Gets metadata about the underlying repository and database server.
     *
     * @return Repository metadata
     */
    RepositoryMetadata getRepositoryMetadata();
}

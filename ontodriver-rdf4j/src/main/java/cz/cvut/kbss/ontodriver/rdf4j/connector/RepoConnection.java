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
package cz.cvut.kbss.ontodriver.rdf4j.connector;

import cz.cvut.kbss.ontodriver.Closeable;
import cz.cvut.kbss.ontodriver.Wrapper;
import cz.cvut.kbss.ontodriver.rdf4j.exception.Rdf4jDriverException;
import org.eclipse.rdf4j.model.IRI;
import org.eclipse.rdf4j.model.Resource;
import org.eclipse.rdf4j.model.Statement;
import org.eclipse.rdf4j.model.Value;
import org.eclipse.rdf4j.model.ValueFactory;

import java.util.Collection;
import java.util.List;
import java.util.Set;

/**
 * A RDF4J repository connection wrapper.
 */
public interface RepoConnection extends Closeable, StatementExecutor, Wrapper {

    /**
     * Explicitly starts a transaction.
     *
     * @throws Rdf4jDriverException If unable to start transaction
     */
    void begin() throws Rdf4jDriverException;

    /**
     * Commits the changes made since transaction beginning.
     *
     * @throws Rdf4jDriverException If an error occurs during commit
     * @see #begin()
     */
    void commit() throws Rdf4jDriverException;

    /**
     * Rolls back changes made since transaction beginning.
     *
     * @throws Rdf4jDriverException If an error occurs when rolling back
     * @see #begin()
     */
    void rollback() throws Rdf4jDriverException;

    /**
     * Puts this connection in read-only mode.
     * <p>
     * Cannot be called during an active transaction.
     *
     * @param readOnly {@code true} enables read-only mode, {@code false} disables it
     */
    void setReadOnly(boolean readOnly);

    /**
     * Checks whether this connection is in a read-only mode.
     *
     * @return {@code true} if this connection is in read-only mode, {@code false} otherwise
     */
    boolean isReadOnly();

    /**
     * Gets resources representing currently existing contexts in the repository.
     *
     * @return List of resources
     * @throws Rdf4jDriverException If repository access error occurs
     */
    List<Resource> getContexts() throws Rdf4jDriverException;

    /**
     * Gets Rdf4j value factory.
     *
     * @return {@link ValueFactory}
     */
    ValueFactory getValueFactory();

    /**
     * Finds statements corresponding to the specified criteria.
     * <p>
     * Note that some of the parameters are optional.
     * <p>
     * This version searches the default context.
     *
     * @param subject         Statement subject, optional
     * @param property        Statement property, optional
     * @param value           Statement value, optional
     * @param includeInferred Whether to include inferred statements as well
     * @return Collection of matching statements
     * @throws Rdf4jDriverException If a repository access error occurs
     * @see #findStatements(Resource, IRI, Value, boolean, Set)
     */
    Collection<Statement> findStatements(Resource subject, IRI property, Value value, boolean includeInferred)
            throws Rdf4jDriverException;

    /**
     * Finds statements corresponding to the specified criteria.
     * <p>
     * Note that some parameters are optional
     *
     * @param subject         Statement subject, optional
     * @param property        Statement property, optional
     * @param value           Statement value, optional
     * @param includeInferred Whether to include inferred statements as well
     * @param contexts        Contexts in which the search should be performed. Empty collection indicates the default
     *                        context will be searched
     * @return Collection of matching statements
     * @throws Rdf4jDriverException If a repository access error occurs
     */
    Collection<Statement> findStatements(Resource subject, IRI property, Value value,
                                         boolean includeInferred, Set<IRI> contexts)
            throws Rdf4jDriverException;

    /**
     * Checks whether the repository contains any statements matching the specified criteria.
     *
     * @param subject         Statement subject, optional
     * @param property        Statement property, optional
     * @param value           Statement value, optional
     * @param includeInferred Whether to include inferred statements as well
     * @param contexts        Optionally specify contexts in which the search should be performed. If empty, the default
     *                        one is used
     * @return Boolean indicating whether the statement exists
     * @throws Rdf4jDriverException If a repository access error occurs
     */
    boolean containsStatement(Resource subject, IRI property, Value value, boolean includeInferred,
                              Set<IRI> contexts)
            throws Rdf4jDriverException;

    /**
     * Checks whether the specified statement is inferred by the repository.
     * <p>
     * Note that given the nature of the RDF4J API, this method will return {@code false} even if the statement is both
     * asserted and inferred, as there is no way to easily ask only for inferred statements but both asserted and
     * inferred statements are returned.
     * <p>
     * Also note that if the repository does not contain the statement at all, {@code false} is returned.
     *
     * @param statement Statement whose inference to check
     * @param contexts  Optionally specify contexts in which the search should be performed. If empty, the default one
     *                  is used
     * @return {@code true} iff the specified statement is inferred in any of the specified contexts
     * @throws Rdf4jDriverException If a repository access error occurs
     */
    boolean isInferred(Statement statement, Set<IRI> contexts) throws Rdf4jDriverException;

    /**
     * Adds the specified statements to the underlying repository.
     * <p>
     * Note that this operation is transactional and the changes are required to be persistent only after successful
     * {@link #commit()}.
     *
     * @param statements The statements to add
     * @throws IllegalStateException If transaction is not active
     * @throws Rdf4jDriverException  If a repository access error occurs
     */
    void addStatements(Collection<Statement> statements) throws Rdf4jDriverException;

    /**
     * Removes the specified statements from the underlying repository.
     * <p>
     * Note that this operation is transactional and the changes are required to be persistent only after successful
     * {@link #commit()}.
     *
     * @param statements The statements to remove
     * @throws IllegalStateException If transaction is not active
     * @throws Rdf4jDriverException  If a repository access error occurs
     */
    void removeStatements(Collection<Statement> statements) throws Rdf4jDriverException;

    /**
     * Removes statements that have the specified subject and predicate pairs.
     *
     * @param spc Subject-predicate-contexts tuples
     * @throws Rdf4jDriverException If a repository access error occurs
     */
    void removePropertyValues(Collection<SubjectPredicateContext> spc) throws Rdf4jDriverException;

    /**
     * Gets the underlying repository product name.
     *
     * @return Product name
     */
    default String getProductName() {
        return "RDF4J";
    }
}

/**
 * Copyright (C) 2016 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.ontodriver.sesame.connector;

import cz.cvut.kbss.ontodriver.Closeable;
import cz.cvut.kbss.ontodriver.Wrapper;
import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;
import org.eclipse.rdf4j.model.*;

import java.util.Collection;
import java.util.List;

public interface Connector extends Closeable, StatementExecutor, Wrapper {

    /**
     * Explicitly starts a transaction.
     *
     * @throws SesameDriverException If unable to start transaction
     */
    void begin() throws SesameDriverException;

    /**
     * Commits the changes made since transaction beginning.
     *
     * @throws SesameDriverException If an error occurs during commit
     * @see #begin()
     */
    void commit() throws SesameDriverException;

    /**
     * Rolls back changes made since transaction beginning.
     *
     * @throws SesameDriverException If an error occurs when rolling back
     * @see #begin()
     */
    void rollback() throws SesameDriverException;

    /**
     * Gets resources representing currently existing contexts in the repository.
     *
     * @return List of resources
     * @throws SesameDriverException If repository access error occurs
     */
    List<Resource> getContexts() throws SesameDriverException;

    /**
     * Gets Sesame value factory.
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
     * @throws SesameDriverException If a repository access error occurs
     * @see #findStatements(Resource, IRI, Value, boolean, IRI)
     */
    Collection<Statement> findStatements(Resource subject, IRI property, Value value, boolean includeInferred)
            throws SesameDriverException;

    /**
     * Finds statements corresponding to the specified criteria.
     * <p>
     * Note that some of the parameters are optional
     *
     * @param subject         Statement subject, optional
     * @param property        Statement property, optional
     * @param value           Statement value, optional
     * @param includeInferred Whether to include inferred statements as well
     * @param context         Optionally specify context in which the search should be performed. If not specified, the
     *                        default one is used
     * @return Collection of matching statements
     * @throws SesameDriverException If a repository access error occurs
     */
    Collection<Statement> findStatements(Resource subject, IRI property, Value value,
                                         boolean includeInferred, IRI context) throws SesameDriverException;

    /**
     * Checks whether the repository contains any statements matching the specified criteria.
     * <p>
     * This version searches the default context.
     *
     * @param subject         Statement subject, optional
     * @param property        Statement property, optional
     * @param value           Statement value, optional
     * @param includeInferred Whether to include inferred statements as well
     * @return Boolean indicating whether the statement exists
     * @throws SesameDriverException If a repository access error occurs
     * @see #containsStatement(Resource, IRI, Value, boolean, IRI)
     */
    boolean containsStatement(Resource subject, IRI property, Value value, boolean includeInferred)
            throws SesameDriverException;

    /**
     * Checks whether the repository contains any statements matching the specified criteria.
     *
     * @param subject         Statement subject, optional
     * @param property        Statement property, optional
     * @param value           Statement value, optional
     * @param includeInferred Whether to include inferred statements as well
     * @param context         Optionally specify context in which the search should be performed. If not specified, the
     *                        default one is used
     * @return Boolean indicating whether the statement exists
     * @throws SesameDriverException If a repository access error occurs
     */
    boolean containsStatement(Resource subject, IRI property, Value value, boolean includeInferred, IRI context)
            throws SesameDriverException;

    /**
     * Adds the specified statements to the underlying repository.
     * <p>
     * Note that this operation is transactional and the changes are required to
     * be persistent only after successful {@link #commit()}.
     *
     * @param statements The statements to add
     * @throws IllegalStateException If transaction is not active
     * @throws SesameDriverException If a repository access error occurs
     */
    void addStatements(Collection<Statement> statements) throws SesameDriverException;

    /**
     * Removes the specified statements from the underlying repository.
     * <p>
     * Note that this operation is transactional and the changes are required to
     * be persistent only after successful {@link #commit()}.
     *
     * @param statements The statements to remove
     * @throws IllegalStateException If transaction is not active
     * @throws SesameDriverException If a repository access error occurs
     */
    void removeStatements(Collection<Statement> statements) throws SesameDriverException;
}

/*
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.ontodriver;

import cz.cvut.kbss.ontodriver.exception.OntoDriverException;

/**
 * This interface represents a SPARQL statement.
 */
public interface Statement extends AutoCloseable {

    /**
     * Specifies which ontology is used for statement evaluation.
     */
    enum StatementOntology {
        /**
         * Transactional ontology. May contain uncommitted changes which influence the statement evaluation.
         */
        TRANSACTIONAL,
        /**
         * The main ontology in the current state. No uncommitted changes are present in it.
         */
        SHARED
    }

    /**
     * Execute the specified SPARQL query.
     *
     * @param sparql The statement to execute
     * @return {@code ResultSet} containing results of the query
     * @throws OntoDriverException If an error occurs during query execution
     */
    ResultSet executeQuery(String sparql) throws OntoDriverException;

    /**
     * Execute the specified SPARQL update query.
     *
     * @param sparql The statement to execute
     * @throws OntoDriverException If an error occurs during query execution
     */
    void executeUpdate(String sparql) throws OntoDriverException;

    /**
     * Sets which ontology is used to evaluate this statement.
     * <p>
     * {@link Statement.StatementOntology#TRANSACTIONAL} ontology is the transactional version. It may contain
     * uncommitted changes and thus the query results may differ from evaluation against {@link
     * Statement.StatementOntology#SHARED}.
     * <p>
     * Note that implementations may ignore this setting depending on their internal transaction management mechanisms.
     *
     * @param ontology Which ontology to use
     */
    void useOntology(StatementOntology ontology);

    /**
     * Gets information about which ontology will be used to evaluate the statement.
     *
     * @return Which ontology will be used for evaluation
     * @see #useOntology(StatementOntology)
     */
    StatementOntology getStatementOntology();

    /**
     * Whether this statement is still open.
     * <p>
     * A {@code Statement} is closed if the method {@link #close()} has been called on it.
     *
     * @return Open status
     */
    boolean isOpen();

    /**
     * Closes this statement, releasing any resources it has hold.
     * <p>
     * Calling the method close on a Statement object that is already closed has no effect.
     * <p>
     * Note: When a {@code Statement} object is closed, its current {@code ResultSet} object, if one exists, is also
     * closed.
     *
     * @throws OntoDriverException If closing the statement fails
     */
    @Override
    void close() throws OntoDriverException;

    /**
     * Disables inference for execution of this statement.
     * <p>
     * Note that by default, inference is enabled for execution of all statements (depending on whether the underlying
     * storage supports inference at all). Also note that in case the underlying repository does not support disabling
     * inference, this configuration may be silently ignored.
     */
    void disableInference();

    /**
     * Checks whether inference is disabled for execution of this statement.
     *
     * @return Inference disabled status
     * @see #disableInference()
     */
    boolean isInferenceDisabled();
}

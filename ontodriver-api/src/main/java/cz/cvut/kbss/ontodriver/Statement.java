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
package cz.cvut.kbss.ontodriver;

import cz.cvut.kbss.ontodriver.exception.OntoDriverException;

import java.net.URI;

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
        CENTRAL
    }

    /**
     * Execute the specified SPARQL query.
     *
     * @param sparql   The statement to execute
     * @param contexts Specifies contexts against which to run the query. Since this parameter is optional, it is
     *                 defined as varargs.
     * @return {@code ResultSet} containing results of the query
     * @throws OntoDriverException If an error occurs during query execution
     */
    ResultSet executeQuery(String sparql, URI... contexts) throws OntoDriverException;

    /**
     * Execute the specified SPARQL update query.
     *
     * @param sparql   The statement to execute
     * @param contexts Specifies contexts against which to run the query. Since this parameter is optional, it is
     *                 defined as varargs.
     * @throws OntoDriverException If an error occurs during query execution
     */
    void executeUpdate(String sparql, URI... contexts) throws OntoDriverException;

    /**
     * Sets which ontology is used to evaluate this statement.
     * <p>
     * {@link Statement.StatementOntology#TRANSACTIONAL} ontology is the transactional
     * snapshot. It may contain uncommitted changes and thus the query results may differ from evaluation against {@link
     * Statement.StatementOntology#CENTRAL}.
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
}

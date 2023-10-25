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
package cz.cvut.kbss.ontodriver.jena.connector;

import cz.cvut.kbss.ontodriver.Statement;
import cz.cvut.kbss.ontodriver.jena.exception.JenaDriverException;
import cz.cvut.kbss.ontodriver.jena.query.AbstractResultSet;
import org.apache.jena.query.Query;

public interface StatementExecutor {

    /**
     * Executes the specified SPARQL SELECT query, returning the Jena ARQ result set.
     * <p>
     * The {@code target} specifies whether the query should be executed on the shared repository or whether the transactional
     * snapshot will be used to evaluate the query. However, some implementations may ignore this parameter.
     *
     * @param query  Query to execute
     * @param target Dataset on which the query should be executed
     * @return ARQ result set
     * @throws JenaDriverException If query execution fails
     */
    AbstractResultSet executeSelectQuery(Query query, Statement.StatementOntology target) throws JenaDriverException;

    /**
     * Executes the specified SPARQL ASK query.
     * <p>
     * The {@code target} specifies whether the query should be executed on the shared repository or whether the transactional
     * snapshot will be used to evaluate the query. However, some implementations may ignore this parameter.
     *
     * @param query  Query to execute
     * @param target Dataset on which the query should be executed
     * @return ASK result
     * @throws JenaDriverException If query execution fails
     */
    AbstractResultSet executeAskQuery(Query query, Statement.StatementOntology target) throws JenaDriverException;

    /**
     * Executes the specified SPARQL 1.1 Update query.
     * <p>
     * The {@code target} specifies whether the query should be executed on the shared repository or whether the transactional
     * snapshot will be used to evaluate the query. However, some implementations may ignore this parameter.
     *
     * @param query  Query to execute
     * @param target Dataset on which the update should be executed
     * @throws JenaDriverException If query execution fails
     */
    void executeUpdate(String query, Statement.StatementOntology target) throws JenaDriverException;
}

/**
 * Copyright (C) 2022 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.ontodriver.rdf4j.connector;

import cz.cvut.kbss.ontodriver.rdf4j.exception.Rdf4jDriverException;
import org.eclipse.rdf4j.query.*;
import org.eclipse.rdf4j.repository.RepositoryConnection;
import org.eclipse.rdf4j.repository.RepositoryException;

/**
 * Actual implementation of statement processing.
 */
class ConnectionStatementExecutor implements StatementExecutor {

    private final RepositoryConnection connection;

    ConnectionStatementExecutor(RepositoryConnection connection) {
        this.connection = connection;
    }

    @Override
    public TupleQueryResult executeSelectQuery(String query) throws Rdf4jDriverException {
        try {
            final TupleQuery tq = connection.prepareTupleQuery(QueryLanguage.SPARQL, query);
            return new QueryResult(tq.evaluate(), connection);
        } catch (MalformedQueryException | QueryEvaluationException | RepositoryException e) {
            throw new Rdf4jDriverException(e);
        }
    }

    @Override
    public boolean executeBooleanQuery(String query) throws Rdf4jDriverException {
        try {
            return connection.prepareBooleanQuery(QueryLanguage.SPARQL, query).evaluate();
        } catch (MalformedQueryException | QueryEvaluationException | RepositoryException e) {
            throw new Rdf4jDriverException(e);
        }
    }

    @Override
    public void executeUpdate(String query) throws Rdf4jDriverException {
        try {
            final Update u = connection.prepareUpdate(QueryLanguage.SPARQL, query);
            u.execute();
        } catch (MalformedQueryException | UpdateExecutionException | RepositoryException e) {
            throw new Rdf4jDriverException(e);
        }
    }
}

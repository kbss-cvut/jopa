/**
 * Copyright (C) 2016 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License along with this program. If not, see
 * <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.ontodriver.sesame.query;

import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.Statement;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.sesame.connector.StatementExecutor;
import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;
import org.eclipse.rdf4j.query.QueryEvaluationException;
import org.eclipse.rdf4j.query.TupleQueryResult;

import java.util.Objects;

public class SesameStatement implements Statement {

    private StatementOntology targetOntology = StatementOntology.TRANSACTIONAL;
    private final StatementExecutor queryExecutor;
    private ResultSet resultSet;

    private boolean open;

    public SesameStatement(StatementExecutor queryExecutor) {
        this.queryExecutor = queryExecutor;
        this.open = true;
    }

    @Override
    public ResultSet executeQuery(String sparql) throws OntoDriverException {
        ensureOpen();
        validateQueryParams(sparql);
        closeCurrentResultSet();
        this.resultSet = determineResult(sparql);
        return resultSet;
    }

    private ResultSet determineResult(String sparql) throws SesameDriverException {
        if (isAskQuery(sparql)) {
            return new AskResultSet(queryExecutor.executeBooleanQuery(sparql), this);
        } else {
            final TupleQueryResult tqr = queryExecutor.executeSelectQuery(sparql);
            try {
                return new SelectResultSet(tqr, this);
            } catch (QueryEvaluationException e) {
                throw new SesameDriverException(e);
            }
        }
    }

    private static boolean isAskQuery(String query) {
        return query.startsWith("ASK");
    }

    @Override
    public void executeUpdate(String sparql) throws OntoDriverException {
        ensureOpen();
        validateQueryParams(sparql);
        closeCurrentResultSet();
        queryExecutor.executeUpdate(sparql);
    }

    @Override
    public void useOntology(StatementOntology ontology) {
        this.targetOntology = ontology;
    }

    @Override
    public StatementOntology getStatementOntology() {
        return targetOntology;
    }

    private static void validateQueryParams(String sparql) {
        Objects.requireNonNull(sparql);
        if (sparql.isEmpty()) {
            throw new IllegalArgumentException("Query string cannot be empty.");
        }
    }

    @Override
    public boolean isOpen() {
        return open;
    }

    @Override
    public void close() throws OntoDriverException {
        if (!open) {
            return;
        }
        this.open = false;
        closeCurrentResultSet();
    }

    private void closeCurrentResultSet() throws OntoDriverException {
        if (resultSet != null) {
            resultSet.close();
            this.resultSet = null;
        }
    }

    public void setUseTransactionalOntology() {
        ensureOpen();
        this.targetOntology = StatementOntology.TRANSACTIONAL;
    }

    public boolean useTransactionalOntology() {
        ensureOpen();
        return targetOntology == StatementOntology.TRANSACTIONAL;
    }

    public void setUseBackupOntology() {
        ensureOpen();
        this.targetOntology = StatementOntology.CENTRAL;
    }

    public boolean useBackupOntology() {
        ensureOpen();
        return targetOntology == StatementOntology.CENTRAL;
    }

    void ensureOpen() {
        if (!open) {
            throw new IllegalStateException("This statement is closed.");
        }
    }
}

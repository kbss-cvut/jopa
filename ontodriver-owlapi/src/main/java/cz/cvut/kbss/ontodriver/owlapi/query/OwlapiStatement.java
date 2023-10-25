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
package cz.cvut.kbss.ontodriver.owlapi.query;

import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.Statement;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.owlapi.OwlapiConnection;

import java.util.Objects;

public class OwlapiStatement implements Statement {

    private StatementOntology targetOntology;
    private boolean open;
    private boolean disableInference;

    private final StatementExecutorFactory executorFactory;
    final OwlapiConnection connection;

    ResultSet resultSet;

    public OwlapiStatement(StatementExecutorFactory executorFactory, OwlapiConnection connection) {
        this.executorFactory = executorFactory;
        this.connection = connection;
        this.open = true;
    }

    void ensureOpen() {
        if (!open) {
            throw new IllegalStateException("The statement is closed.");
        }
    }

    @Override
    public ResultSet executeQuery(String sparql) throws OntoDriverException {
        ensureOpen();
        Objects.requireNonNull(sparql);
        closeExistingResultSet();
        this.resultSet = getExecutor().executeQuery(querySpec(sparql));
        return resultSet;
    }

    StatementExecutor getExecutor() {
        return executorFactory.getStatementExecutor(targetOntology);
    }

    QuerySpecification querySpec(String query) {
        return QuerySpecification.query(query).disableInference(disableInference).statement(this);
    }

    @Override
    public void executeUpdate(String sparql) throws OntoDriverException {
        ensureOpen();
        Objects.requireNonNull(sparql);
        closeExistingResultSet();
        getExecutor().executeUpdate(querySpec(sparql));
        connection.commitIfAuto();
    }

    @Override
    public void useOntology(StatementOntology ontology) {
        this.targetOntology = ontology;
    }

    @Override
    public StatementOntology getStatementOntology() {
        return targetOntology;
    }


    @Override
    public boolean isOpen() {
        return open;
    }

    @Override
    public void close() throws OntoDriverException {
        this.open = false;
        closeExistingResultSet();
    }

    @Override
    public void disableInference() {
        this.disableInference = true;
    }

    @Override
    public boolean isInferenceDisabled() {
        return disableInference;
    }

    void closeExistingResultSet() throws OntoDriverException {
        if (resultSet != null) {
            resultSet.close();
            this.resultSet = null;
        }
    }
}

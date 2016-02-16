/**
 * Copyright (C) 2011 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.owlapi.query;

import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.owlapi.OwlapiConnection;
import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.Statement;

import java.net.URI;
import java.util.Objects;

public class OwlapiStatement implements Statement {

    private StatementOntology targetOntology;
    private boolean open;

    private final StatementExecutorFactory executorFactory;
    final OwlapiConnection connection;

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
    public ResultSet executeQuery(String sparql, URI... contexts) throws OntoDriverException {
        ensureOpen();
        Objects.requireNonNull(sparql);
        return getExecutor().executeQuery(sparql, this);
    }

    StatementExecutor getExecutor() {
        return executorFactory.getStatementExecutor(targetOntology);
    }

    @Override
    public void executeUpdate(String sparql, URI... contexts) throws OntoDriverException {
        ensureOpen();
        Objects.requireNonNull(sparql);
        getExecutor().executeUpdate(sparql);
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
    public void close() throws Exception {
        this.open = false;
    }
}

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
package cz.cvut.kbss.ontodriver.jena.query;

import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.Statement;
import cz.cvut.kbss.ontodriver.jena.connector.StatementExecutor;
import cz.cvut.kbss.ontodriver.jena.exception.JenaDriverException;
import org.apache.jena.query.Query;
import org.apache.jena.query.QueryFactory;
import org.apache.jena.query.QueryParseException;

import java.util.Objects;

public class JenaStatement implements Statement {

    private final StatementExecutor executor;

    private StatementOntology targetOntology = StatementOntology.TRANSACTIONAL;
    private boolean open = true;

    private AbstractResultSet currentResultSet;

    public JenaStatement(StatementExecutor executor) {
        this.executor = executor;
    }

    @Override
    public ResultSet executeQuery(String sparql) throws JenaDriverException {
        ensureOpen();
        final Query query = parseQuery(Objects.requireNonNull(sparql));
        closeCurrentResultSet();
        final AbstractResultSet resultSet;
        if (query.isAskType()) {
            resultSet = executor.executeAskQuery(query, targetOntology);
        } else {
            resultSet = executor.executeSelectQuery(query, targetOntology);
        }
        resultSet.setStatement(this);
        this.currentResultSet = resultSet;
        return resultSet;
    }

    void ensureOpen() {
        if (!open) {
            throw new IllegalStateException("Statement is closed.");
        }
    }

    private static Query parseQuery(String sparql) throws JenaDriverException {
        try {
            return QueryFactory.create(sparql);
        } catch (QueryParseException e) {
            throw new JenaDriverException("Unable to parse query " + sparql, e);
        }
    }

    private void closeCurrentResultSet() throws JenaDriverException {
        if (currentResultSet != null) {
            currentResultSet.close();
            this.currentResultSet = null;
        }
    }

    @Override
    public void executeUpdate(String sparql) throws JenaDriverException {
        ensureOpen();
        Objects.requireNonNull(sparql);
        closeCurrentResultSet();
        executor.executeUpdate(sparql, targetOntology);
    }

    @Override
    public void useOntology(StatementOntology ontology) {
        ensureOpen();
        this.targetOntology = Objects.requireNonNull(ontology);
    }

    @Override
    public StatementOntology getStatementOntology() {
        return targetOntology;
    }

    @Override
    public void close() throws JenaDriverException {
        this.open = false;
        closeCurrentResultSet();
    }

    @Override
    public void disableInference() {
        // TODO
    }

    @Override
    public boolean isInferenceDisabled() {
        // TODO
        return false;
    }

    @Override
    public boolean isOpen() {
        return open;
    }
}

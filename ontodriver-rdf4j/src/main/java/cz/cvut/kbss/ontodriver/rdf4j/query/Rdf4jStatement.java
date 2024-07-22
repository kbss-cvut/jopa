/*
 * JOPA
 * Copyright (C) 2024 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.rdf4j.query;

import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.Statement;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.rdf4j.connector.StatementExecutor;
import cz.cvut.kbss.ontodriver.rdf4j.exception.Rdf4jDriverException;
import org.eclipse.rdf4j.query.MalformedQueryException;
import org.eclipse.rdf4j.query.QueryEvaluationException;
import org.eclipse.rdf4j.query.QueryLanguage;
import org.eclipse.rdf4j.query.TupleQueryResult;
import org.eclipse.rdf4j.query.parser.ParsedBooleanQuery;
import org.eclipse.rdf4j.query.parser.QueryParserUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Objects;

public class Rdf4jStatement implements Statement {

    private static final Logger LOG = LoggerFactory.getLogger(Rdf4jStatement.class);

    private boolean inferenceDisabled = false;
    private final StatementExecutor queryExecutor;
    private ResultSet resultSet;

    private boolean open;

    public Rdf4jStatement(StatementExecutor queryExecutor) {
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

    private ResultSet determineResult(String sparql) throws Rdf4jDriverException {
        if (isAskQuery(sparql)) {
            return new AskResultSet(queryExecutor.executeBooleanQuery(querySpec(sparql)), this);
        } else {
            final TupleQueryResult tqr = queryExecutor.executeSelectQuery(querySpec(sparql));
            try {
                return new SelectResultSet(tqr, this);
            } catch (QueryEvaluationException e) {
                throw new Rdf4jDriverException(e);
            }
        }
    }

    QuerySpecification querySpec(String sparql) {
        return QuerySpecification.query(sparql).includeInference(!inferenceDisabled);
    }

    private static boolean isAskQuery(String query) throws Rdf4jDriverException {
        try {
            return QueryParserUtil.parseOperation(QueryLanguage.SPARQL, query, null) instanceof ParsedBooleanQuery;
        } catch (MalformedQueryException e) {
            throw new Rdf4jDriverException("Invalid query \"" + query + "\".", e);
        }
    }

    @Override
    public void executeUpdate(String sparql) throws OntoDriverException {
        ensureOpen();
        validateQueryParams(sparql);
        closeCurrentResultSet();
        queryExecutor.executeUpdate(querySpec(sparql));
    }

    @Override
    public void useOntology(StatementOntology ontology) {
        LOG.warn("RDF4J driver does not support changing the target ontology because it does not use transactional data snapshots.");
    }

    @Override
    public StatementOntology getStatementOntology() {
        return StatementOntology.SHARED;
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

    @Override
    public void disableInference() {
        this.inferenceDisabled = true;
    }

    @Override
    public boolean isInferenceDisabled() {
        return inferenceDisabled;
    }

    void ensureOpen() {
        if (!open) {
            throw new IllegalStateException("This statement is closed.");
        }
    }
}

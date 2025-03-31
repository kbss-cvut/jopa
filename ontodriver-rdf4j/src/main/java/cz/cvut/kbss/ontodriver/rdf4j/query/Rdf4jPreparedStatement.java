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
package cz.cvut.kbss.ontodriver.rdf4j.query;

import cz.cvut.kbss.ontodriver.PreparedStatement;
import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.rdf4j.connector.StatementExecutor;
import cz.cvut.kbss.ontodriver.util.StatementHolder;

import java.util.Objects;

public class Rdf4jPreparedStatement extends Rdf4jStatement implements PreparedStatement {

    private final StatementHolder statementHolder;

    public Rdf4jPreparedStatement(StatementExecutor executor, String statement) {
        super(executor);
        this.statementHolder = new StatementHolder(statement);
        if (statementHolder.getStatement().isEmpty()) {
            throw new IllegalArgumentException("The statement string cannot be empty.");
        }
        statementHolder.analyzeStatement();
    }

    @Override
    public void setObject(String binding, Object value) {
        ensureOpen();
        Objects.requireNonNull(value);
        statementHolder.setParameter(binding, value.toString());
    }

    @Override
    public ResultSet executeQuery() throws OntoDriverException {
        ensureOpen();
        return executeQuery(statementHolder.assembleStatement());
    }

    @Override
    public void executeUpdate() throws OntoDriverException {
        ensureOpen();
        executeUpdate(statementHolder.assembleStatement());
    }

    @Override
    public void clearParameters() {
        statementHolder.clearParameters();
    }
}

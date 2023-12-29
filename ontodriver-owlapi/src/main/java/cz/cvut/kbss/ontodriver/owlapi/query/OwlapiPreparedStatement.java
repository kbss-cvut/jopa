/*
 * JOPA
 * Copyright (C) 2023 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.owlapi.query;

import cz.cvut.kbss.ontodriver.PreparedStatement;
import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.owlapi.OwlapiConnection;
import cz.cvut.kbss.ontodriver.util.StatementHolder;

import java.util.Objects;

import static cz.cvut.kbss.ontodriver.util.ErrorUtils.getNPXMessageSupplier;

public class OwlapiPreparedStatement extends OwlapiStatement implements PreparedStatement {

    private final StatementHolder statementHolder;

    public OwlapiPreparedStatement(StatementExecutorFactory executorFactory, OwlapiConnection connection,
                                   String statement) {
        super(executorFactory, connection);
        this.statementHolder = new StatementHolder(statement);
        if (statementHolder.getStatement().isEmpty()) {
            throw new IllegalArgumentException("Statement cannot be empty.");
        }
        statementHolder.analyzeStatement();
    }

    @Override
    public ResultSet executeQuery() throws OntoDriverException {
        ensureOpen();
        closeExistingResultSet();
        this.resultSet = getExecutor().executeQuery(querySpec(statementHolder.assembleStatement()));
        return resultSet;
    }

    @Override
    public void executeUpdate() throws OntoDriverException {
        ensureOpen();
        getExecutor().executeUpdate(querySpec(statementHolder.assembleStatement()));
        connection.commitIfAuto();
    }

    @Override
    public void setObject(String binding, Object value) {
        ensureOpen();
        Objects.requireNonNull(binding, getNPXMessageSupplier("binding"));
        Objects.requireNonNull(value, getNPXMessageSupplier("value"));
        statementHolder.setParameter(binding, value.toString());
    }

    @Override
    public void clearParameters() {
        ensureOpen();
        statementHolder.clearParameters();
    }
}

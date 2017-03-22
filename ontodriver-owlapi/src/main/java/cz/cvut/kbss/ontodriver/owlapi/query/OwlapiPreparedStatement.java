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
        return getExecutor().executeQuery(statementHolder.assembleStatement(), this);
    }

    @Override
    public void executeUpdate() throws OntoDriverException {
        ensureOpen();
        getExecutor().executeUpdate(statementHolder.assembleStatement());
        connection.commitIfAuto();
    }

    @Override
    public void setObject(String binding, Object value) throws OntoDriverException {
        ensureOpen();
        Objects.requireNonNull(binding, getNPXMessageSupplier("binding"));
        Objects.requireNonNull(value, getNPXMessageSupplier("value"));
        statementHolder.setParameter(binding, value.toString());
    }

    @Override
    public void clearParameters() throws OntoDriverException {
        ensureOpen();
        statementHolder.clearParameters();
    }
}

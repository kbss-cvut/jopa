/**
 * Copyright (C) 2019 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.jena.query;

import cz.cvut.kbss.ontodriver.PreparedStatement;
import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.jena.connector.StatementExecutor;
import cz.cvut.kbss.ontodriver.jena.exception.JenaDriverException;
import cz.cvut.kbss.ontodriver.util.StatementHolder;

import java.util.Objects;

public class JenaPreparedStatement extends JenaStatement implements PreparedStatement {

    private final StatementHolder holder;

    public JenaPreparedStatement(StatementExecutor executor, String sparql) {
        super(executor);
        this.holder = new StatementHolder(sparql);
        if (holder.getStatement().isEmpty()) {
            throw new IllegalArgumentException("Statement cannot be empty.");
        }
        holder.analyzeStatement();
    }

    @Override
    public ResultSet executeQuery() throws JenaDriverException {
        ensureOpen();
        return executeQuery(holder.assembleStatement());
    }

    @Override
    public void executeUpdate() throws JenaDriverException {
        ensureOpen();
        executeUpdate(holder.assembleStatement());
    }

    @Override
    public void setObject(String binding, Object value) {
        ensureOpen();
        Objects.requireNonNull(binding);
        Objects.requireNonNull(value);
        holder.setParameter(binding, value.toString());
    }

    @Override
    public void clearParameters() {
        ensureOpen();
        holder.clearParameters();
    }
}

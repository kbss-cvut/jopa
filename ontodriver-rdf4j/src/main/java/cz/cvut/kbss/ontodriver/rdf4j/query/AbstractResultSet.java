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
package cz.cvut.kbss.ontodriver.rdf4j.query;

import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.Statement;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;

import java.util.NoSuchElementException;

abstract class AbstractResultSet implements ResultSet {

    private final Statement statement;

    private int index;
    private boolean open;

    AbstractResultSet(Statement statement) {
        assert statement != null;

        this.statement = statement;
        this.index = -1;
        this.open = true;
    }

    @Override
    public boolean isOpen() {
        return open;
    }

    @Override
    public int getRowIndex() {
        ensureOpen();
        return index;
    }

    @Override
    public Statement getStatement() {
        ensureOpen();
        return statement;
    }

    @Override
    public boolean isFirst() {
        ensureOpen();
        return index == 0;
    }

    void ensureOpen() {
        if (!open) {
            throw new IllegalStateException("The result set is closed!");
        }
    }

    @Override
    public void close() throws OntoDriverException {
        this.open = false;
    }

    @Override
    public void first() {
        throw new UnsupportedOperationException(
                "Returning to the first row is not supported by this result set.");
    }

    @Override
    public void previous() {
        throw new UnsupportedOperationException("Going back is not supported by this result set.");
    }

    @Override
    public void relative(int rows) throws OntoDriverException {
        setRowIndex(index + rows);
    }

    @Override
    public void last() throws OntoDriverException {
        ensureOpen();
        while (hasNext()) {
            next();
        }
    }

    @Override
    public void next() throws OntoDriverException {
        ensureOpen();
        if (!hasNext()) {
            throw new NoSuchElementException();
        }
        index++;
    }

    @Override
    public void setRowIndex(int rowIndex) throws OntoDriverException {
        ensureOpen();
        if (rowIndex < index) {
            throw new UnsupportedOperationException(
                    "Going back in this result set is not supported.");
        }
        if (rowIndex == index) {
            return;
        }
        while (index <= rowIndex) {
            next();
        }
    }
}

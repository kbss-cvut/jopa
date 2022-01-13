/**
 * Copyright (C) 2022 Czech Technical University in Prague
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

import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.Statement;
import cz.cvut.kbss.ontodriver.jena.exception.JenaDriverException;

import java.util.NoSuchElementException;
import java.util.Observer;

public abstract class AbstractResultSet implements ResultSet {

    private boolean open = true;
    private int rowIndex = -1;

    private JenaStatement statement;

    void setStatement(JenaStatement statement) {
        this.statement = statement;
    }

    void ensureOpen() {
        if (!open) {
            throw new IllegalStateException("This result set is closed.");
        }
    }

    @Override
    public Statement getStatement() {
        ensureOpen();
        assert statement != null;
        return statement;
    }

    @Override
    public boolean isFirst() {
        ensureOpen();
        return rowIndex == 0;
    }

    @Override
    public void last() {
        ensureOpen();
        while (hasNext()) {
            next();
        }
    }

    @Override
    public void first() {
        throw new UnsupportedOperationException("This result set does not support returning to the first rowIndex.");
    }

    void ensureState() {
        ensureOpen();
        if (getRowIndex() < 0) {
            throw new IllegalStateException("Must call next before getting any values.");
        }
    }

    @Override
    public boolean hasNext() {
        // Intended for overriding
        ensureOpen();
        return false;
    }

    @Override
    public void next() {
        ensureOpen();
        if (!hasNext()) {
            throw new NoSuchElementException("No more rows found in this result set.");
        }
        this.rowIndex++;
    }

    @Override
    public void previous() {
        throw new UnsupportedOperationException("Moving back is not supported by this result set.");
    }

    @Override
    public void registerObserver(Observer observer) {
        throw new UnsupportedOperationException("Not supported by the current version.");
    }

    @Override
    public void relative(int rows) {
        ensureOpen();
        setRowIndex(rowIndex + rows);
    }

    @Override
    public int getRowIndex() {
        return rowIndex;
    }

    @Override
    public void setRowIndex(int newIndex) {
        ensureOpen();
        if (newIndex < rowIndex) {
            throw new UnsupportedOperationException("Moving back is not supported by this result set.");
        }
        while (rowIndex < newIndex) {
            next();
        }
    }

    @Override
    public void close() throws JenaDriverException {
        this.open = false;
    }

    @Override
    public boolean isOpen() {
        return open;
    }
}

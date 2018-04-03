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

import cz.cvut.kbss.ontodriver.Statement;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.owl2query.model.QueryResult;
import org.semanticweb.owlapi.model.OWLObject;

import java.util.Observer;

class AskResultSet extends AbstractResultSet {

    private final boolean result;
    private boolean nextCalled = false;

    public AskResultSet(QueryResult<OWLObject> queryResult, Statement statement) {
        super(statement);
        this.result = !queryResult.isEmpty();
    }

    @Override
    public int findColumn(String columnLabel) {
        return 0;
    }

    @Override
    public int getColumnCount() {
        return 1;
    }

    @Override
    public boolean isBound(int variableIndex) {
        ensureState();
        // We do not care about index/name in ASK result set
        return true;
    }

    @Override
    public boolean isBound(String variableName) {
        ensureState();
        // We do not care about index/name in ASK result set
        return true;
    }

    @Override
    public void first() throws OntoDriverException {
        // Do nothing
    }

    @Override
    public boolean getBoolean(int columnIndex) throws OntoDriverException {
        ensureState();
        return result;
    }

    private void ensureState() {
        ensureOpen();
        if (!nextCalled) {
            throw new IllegalStateException(
                    "Next has to be called before attempting to get any value from the result set.");
        }
    }

    @Override
    public boolean getBoolean(String columnLabel) throws OntoDriverException {
        ensureState();
        return result;
    }

    @Override
    public byte getByte(int columnIndex) throws OntoDriverException {
        ensureState();
        throw unsupported("byte");
    }

    private UnsupportedOperationException unsupported(String type) {
        return new UnsupportedOperationException("ASK query results cannot return " + type + "values.");
    }

    @Override
    public byte getByte(String columnLabel) throws OntoDriverException {
        ensureState();
        throw unsupported("byte");
    }

    @Override
    public double getDouble(int columnIndex) throws OntoDriverException {
        ensureState();
        throw unsupported("double");
    }

    @Override
    public double getDouble(String columnLabel) throws OntoDriverException {
        ensureState();
        throw unsupported("double");
    }

    @Override
    public float getFloat(int columnIndex) throws OntoDriverException {
        ensureState();
        throw unsupported("float");
    }

    @Override
    public float getFloat(String columnLabel) throws OntoDriverException {
        ensureState();
        throw unsupported("float");
    }

    @Override
    public int getInt(int columnIndex) throws OntoDriverException {
        ensureState();
        throw unsupported("int");
    }

    @Override
    public int getInt(String columnLabel) throws OntoDriverException {
        ensureState();
        throw unsupported("int");
    }

    @Override
    public long getLong(int columnIndex) throws OntoDriverException {
        ensureState();
        throw unsupported("long");
    }

    @Override
    public long getLong(String columnLabel) throws OntoDriverException {
        ensureState();
        throw unsupported("long");
    }

    @Override
    public Object getObject(int columnIndex) throws OntoDriverException {
        ensureState();
        return result;
    }

    @Override
    public Object getObject(String columnLabel) throws OntoDriverException {
        ensureState();
        return result;
    }

    @Override
    public <T> T getObject(int columnIndex, Class<T> cls) throws OntoDriverException {
        ensureState();
        if (cls.isAssignableFrom(Boolean.class)) {
            return cls.cast(result);
        }
        if (cls.isAssignableFrom(String.class)) {
            return cls.cast(Boolean.toString(result));
        }
        throw unsupported(cls.getSimpleName());
    }

    @Override
    public <T> T getObject(String columnLabel, Class<T> cls) throws OntoDriverException {
        return getObject(0, cls);
    }

    @Override
    public int getRowIndex() throws OntoDriverException {
        return 0;
    }

    @Override
    public short getShort(int columnIndex) throws OntoDriverException {
        ensureState();
        throw unsupported("short");
    }

    @Override
    public short getShort(String columnLabel) throws OntoDriverException {
        ensureState();
        throw unsupported("short");
    }

    @Override
    public String getString(int columnIndex) throws OntoDriverException {
        ensureState();
        return Boolean.toString(result);
    }

    @Override
    public String getString(String columnLabel) throws OntoDriverException {
        ensureState();
        return Boolean.toString(result);
    }

    @Override
    public boolean isFirst() throws OntoDriverException {
        return true;
    }

    @Override
    public boolean hasNext() throws OntoDriverException {
        return !nextCalled;
    }

    @Override
    public void last() throws OntoDriverException {
        // do nothing
    }

    @Override
    public void next() throws OntoDriverException {
        if (!hasNext()) {
            throw new IllegalStateException("No more elements in this result set.");
        }
        this.nextCalled = true;
    }

    @Override
    public void previous() throws OntoDriverException {
        // Do nothing
    }

    @Override
    public void registerObserver(Observer observer) throws OntoDriverException {

    }

    @Override
    public void relative(int rows) throws OntoDriverException {
        throw new UnsupportedOperationException("Row-based navigation not supported by ASK result set.");
    }

    @Override
    public void setRowIndex(int rowIndex) throws OntoDriverException {
        throw new UnsupportedOperationException("Row-based navigation not supported by ASK result set.");
    }
}

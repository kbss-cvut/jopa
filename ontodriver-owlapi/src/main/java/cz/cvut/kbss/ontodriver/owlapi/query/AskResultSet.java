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
package cz.cvut.kbss.ontodriver.owlapi.query;

import cz.cvut.kbss.ontodriver.Statement;
import cz.cvut.kbss.owl2query.model.QueryResult;
import org.semanticweb.owlapi.model.OWLObject;

import java.util.List;

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

    /**
     * Always returns a list with a single element "x".
     * <p>
     * Any column name will resolve to the boolean result represented by this result set.
     *
     * @return List of size one element - "x"
     */
    @Override
    public List<String> getColumnNames() {
        return List.of("x");
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
    public void first() {
        // Do nothing
    }

    @Override
    public boolean getBoolean(int columnIndex) {
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
    public boolean getBoolean(String columnLabel) {
        ensureState();
        return result;
    }

    @Override
    public byte getByte(int columnIndex) {
        ensureState();
        throw unsupported("byte");
    }

    private static UnsupportedOperationException unsupported(String type) {
        return new UnsupportedOperationException("ASK query results cannot return " + type + "values.");
    }

    @Override
    public byte getByte(String columnLabel) {
        ensureState();
        throw unsupported("byte");
    }

    @Override
    public double getDouble(int columnIndex) {
        ensureState();
        throw unsupported("double");
    }

    @Override
    public double getDouble(String columnLabel) {
        ensureState();
        throw unsupported("double");
    }

    @Override
    public float getFloat(int columnIndex) {
        ensureState();
        throw unsupported("float");
    }

    @Override
    public float getFloat(String columnLabel) {
        ensureState();
        throw unsupported("float");
    }

    @Override
    public int getInt(int columnIndex) {
        ensureState();
        throw unsupported("int");
    }

    @Override
    public int getInt(String columnLabel) {
        ensureState();
        throw unsupported("int");
    }

    @Override
    public long getLong(int columnIndex) {
        ensureState();
        throw unsupported("long");
    }

    @Override
    public long getLong(String columnLabel) {
        ensureState();
        throw unsupported("long");
    }

    @Override
    public Object getObject(int columnIndex) {
        ensureState();
        return result;
    }

    @Override
    public Object getObject(String columnLabel) {
        ensureState();
        return result;
    }

    @Override
    public <T> T getObject(int columnIndex, Class<T> cls) {
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
    public <T> T getObject(String columnLabel, Class<T> cls) {
        return getObject(0, cls);
    }

    @Override
    public int getRowIndex() {
        return 0;
    }

    @Override
    public short getShort(int columnIndex) {
        ensureState();
        throw unsupported("short");
    }

    @Override
    public short getShort(String columnLabel) {
        ensureState();
        throw unsupported("short");
    }

    @Override
    public String getString(int columnIndex) {
        ensureState();
        return Boolean.toString(result);
    }

    @Override
    public String getString(String columnLabel) {
        ensureState();
        return Boolean.toString(result);
    }

    @Override
    public boolean isFirst() {
        return true;
    }

    @Override
    public boolean hasNext() {
        return !nextCalled;
    }

    @Override
    public void last() {
        // do nothing
    }

    @Override
    public void next() {
        if (!hasNext()) {
            throw new IllegalStateException("No more elements in this result set.");
        }
        this.nextCalled = true;
    }

    @Override
    public void previous() {
        // Do nothing
    }


    @Override
    public void relative(int rows) {
        throw new UnsupportedOperationException("Row-based navigation not supported by ASK result set.");
    }

    @Override
    public void setRowIndex(int rowIndex) {
        throw new UnsupportedOperationException("Row-based navigation not supported by ASK result set.");
    }
}

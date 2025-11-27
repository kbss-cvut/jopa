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

import cz.cvut.kbss.ontodriver.Statement;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.rdf4j.exception.Rdf4jDriverException;

import java.util.List;

public class AskResultSet extends AbstractResultSet {

    private final boolean result;

    public AskResultSet(boolean result, Statement statement) {
        super(statement);
        this.result = result;
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
        // Return always true, as we do not care about index/name for ASK results
        return true;
    }

    @Override
    public boolean isBound(String variableName) {
        // Return always true, as we do not care about index/name for ASK results
        return true;
    }

    // We discard column index and column name, because in a boolean result, there is no such concept. Therefore,
    // the result is returned for any column index and column name.

    @Override
    public boolean getBoolean(int columnIndex) {
        ensureState();
        return result;
    }

    private void ensureState() {
        ensureOpen();
        if (!isFirst()) {
            throw new IllegalStateException("Must call next before getting the first value.");
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
    public <T> T getObject(int columnIndex, Class<T> cls) throws OntoDriverException {
        ensureState();
        return toType(cls);
    }

    @Override
    public <T> T getObject(String columnLabel, Class<T> cls) throws OntoDriverException {
        ensureState();
        return toType(cls);
    }

    private <T> T toType(Class<T> type) throws Rdf4jDriverException {
        if (type.isAssignableFrom(Boolean.class)) {
            return type.cast(result);
        }
        if (type.isAssignableFrom(String.class)) {
            return type.cast(getString(0));
        }
        throw new Rdf4jDriverException("Unable to return boolean result as type " + type);
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
    public boolean hasNext() throws OntoDriverException {
        ensureOpen();
        return !isFirst();
    }
}

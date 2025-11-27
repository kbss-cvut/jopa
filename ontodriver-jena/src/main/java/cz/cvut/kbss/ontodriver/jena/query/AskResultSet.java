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
package cz.cvut.kbss.ontodriver.jena.query;

import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.jena.exception.JenaDriverException;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.List;
import java.util.Objects;

public class AskResultSet extends AbstractResultSet {

    private final boolean result;

    public AskResultSet(boolean result) {
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
        // ASK query variables are always bound
        return true;
    }

    @Override
    public boolean isBound(String variableName) {
        // ASK query variables are always bound
        return true;
    }

    // We discard column index and column name, because in a boolean result, there is no such concept. Therefore,
    // the result is returned for any column index and column name.

    @Override
    public boolean getBoolean(int columnIndex) {
        ensureState();
        return result;
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
        return new UnsupportedOperationException("ASK query results cannot return " + type + " values.");
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
        return toObject(cls);
    }

    private <T> T toObject(Class<T> cls) throws JenaDriverException {
        ensureState();
        Objects.requireNonNull(cls);
        if (cls.isAssignableFrom(Boolean.class)) {
            return cls.cast(result);
        }
        if (cls.isAssignableFrom(String.class)) {
            return cls.cast(Boolean.toString(result));
        }
        return buildUsingConstructor(cls);
    }

    private <T> T buildUsingConstructor(Class<T> cls) throws JenaDriverException {
        try {
            for (Constructor<?> c : cls.getDeclaredConstructors()) {
                if (c.getParameterCount() != 1) {
                    continue;
                }
                final Class<?> paramType = c.getParameterTypes()[0];
                if (paramType.isAssignableFrom(Boolean.class) || paramType.isAssignableFrom(boolean.class)) {
                    return cls.cast(c.newInstance(result));
                } else if (paramType.isAssignableFrom(String.class)) {
                    return cls.cast(c.newInstance(Boolean.toString(result)));
                }
            }
        } catch (IllegalAccessException | InstantiationException | InvocationTargetException e) {
            throw new JenaDriverException("Unable to instantiate class " + cls + " with value " + result, e);
        }
        throw new JenaDriverException("No suitable constructor for value " + result + " found in type " + cls);
    }

    @Override
    public <T> T getObject(String columnLabel, Class<T> cls) throws OntoDriverException {
        return toObject(cls);
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
    public boolean hasNext() {
        ensureOpen();
        return getRowIndex() == -1;
    }
}

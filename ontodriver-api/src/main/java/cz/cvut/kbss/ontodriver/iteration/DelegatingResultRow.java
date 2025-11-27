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
package cz.cvut.kbss.ontodriver.iteration;

import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;

import java.util.List;
import java.util.Objects;

/**
 * {@link ResultRow} implementation which delegates retrieval calls to the underlying
 * {@link cz.cvut.kbss.ontodriver.ResultSet}.
 */
public class DelegatingResultRow implements ResultRow {

    private final ResultSet resultSet;

    public DelegatingResultRow(ResultSet resultSet) {
        this.resultSet = Objects.requireNonNull(resultSet);
    }

    @Override
    public int findColumn(String columnLabel) {
        return resultSet.findColumn(columnLabel);
    }

    @Override
    public int getColumnCount() {
        return resultSet.getColumnCount();
    }

    @Override
    public List<String> getColumnNames() {
        return resultSet.getColumnNames();
    }

    @Override
    public boolean isBound(int variableIndex) throws OntoDriverException {
        return resultSet.isBound(variableIndex);
    }

    @Override
    public boolean isBound(String variableName) throws OntoDriverException {
        return resultSet.isBound(variableName);
    }

    @Override
    public boolean getBoolean(int columnIndex) throws OntoDriverException {
        return resultSet.getBoolean(columnIndex);
    }

    @Override
    public boolean getBoolean(String columnLabel) throws OntoDriverException {
        return resultSet.getBoolean(columnLabel);
    }

    @Override
    public byte getByte(int columnIndex) throws OntoDriverException {
        return resultSet.getByte(columnIndex);
    }

    @Override
    public byte getByte(String columnLabel) throws OntoDriverException {
        return resultSet.getByte(columnLabel);
    }

    @Override
    public double getDouble(int columnIndex) throws OntoDriverException {
        return resultSet.getDouble(columnIndex);
    }

    @Override
    public double getDouble(String columnLabel) throws OntoDriverException {
        return resultSet.getDouble(columnLabel);
    }

    @Override
    public float getFloat(int columnIndex) throws OntoDriverException {
        return resultSet.getFloat(columnIndex);
    }

    @Override
    public float getFloat(String columnLabel) throws OntoDriverException {
        return resultSet.getFloat(columnLabel);
    }

    @Override
    public int getInt(int columnIndex) throws OntoDriverException {
        return resultSet.getInt(columnIndex);
    }

    @Override
    public int getInt(String columnLabel) throws OntoDriverException {
        return resultSet.getInt(columnLabel);
    }

    @Override
    public long getLong(int columnIndex) throws OntoDriverException {
        return resultSet.getLong(columnIndex);
    }

    @Override
    public long getLong(String columnLabel) throws OntoDriverException {
        return resultSet.getLong(columnLabel);
    }

    @Override
    public Object getObject(int columnIndex) throws OntoDriverException {
        return resultSet.getObject(columnIndex);
    }

    @Override
    public Object getObject(String columnLabel) throws OntoDriverException {
        return resultSet.getObject(columnLabel);
    }

    @Override
    public <T> T getObject(int columnIndex, Class<T> cls) throws OntoDriverException {
        return resultSet.getObject(columnIndex, cls);
    }

    @Override
    public <T> T getObject(String columnLabel, Class<T> cls) throws OntoDriverException {
        return resultSet.getObject(columnLabel, cls);
    }

    @Override
    public short getShort(int columnIndex) throws OntoDriverException {
        return resultSet.getShort(columnIndex);
    }

    @Override
    public short getShort(String columnLabel) throws OntoDriverException {
        return resultSet.getShort(columnLabel);
    }

    @Override
    public String getString(int columnIndex) throws OntoDriverException {
        return resultSet.getString(columnIndex);
    }

    @Override
    public String getString(String columnLabel) throws OntoDriverException {
        return resultSet.getString(columnLabel);
    }

    @Override
    public int getIndex() throws OntoDriverException {
        return resultSet.getRowIndex();
    }
}

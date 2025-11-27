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

import cz.cvut.kbss.jopa.datatype.DatatypeTransformer;
import cz.cvut.kbss.jopa.datatype.exception.UnsupportedTypeTransformationException;
import cz.cvut.kbss.ontodriver.Statement;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.exception.VariableNotBoundException;
import cz.cvut.kbss.ontodriver.rdf4j.exception.Rdf4jDriverException;
import cz.cvut.kbss.ontodriver.rdf4j.util.Rdf4jUtils;
import org.eclipse.rdf4j.model.IRI;
import org.eclipse.rdf4j.model.Literal;
import org.eclipse.rdf4j.model.Value;
import org.eclipse.rdf4j.query.BindingSet;
import org.eclipse.rdf4j.query.QueryEvaluationException;
import org.eclipse.rdf4j.query.TupleQueryResult;

import java.util.Collections;
import java.util.List;
import java.util.Objects;

// TODO Resolve mapping of data values with language tag
public class SelectResultSet extends AbstractResultSet {

    private final TupleQueryResult result;
    private List<String> bindings;
    private BindingSet current;

    public SelectResultSet(TupleQueryResult result, Statement statement) {
        super(statement);
        assert result != null;

        this.result = result;
        init();
    }

    private void init() {
        this.bindings = result.getBindingNames();
    }

    @Override
    public void close() throws OntoDriverException {
        try {
            result.close();
        } catch (QueryEvaluationException e) {
            throw new OntoDriverException(e);
        } finally {
            super.close();
        }
    }

    @Override
    public int findColumn(String columnLabel) {
        ensureOpen();
        return bindings.indexOf(columnLabel);
    }

    @Override
    public int getColumnCount() {
        ensureOpen();
        return bindings.size();
    }

    @Override
    public List<String> getColumnNames() {
        return Collections.unmodifiableList(bindings);
    }

    @Override
    public boolean isBound(int variableIndex) {
        return variableIndex >= 0 && variableIndex < bindings.size() && current
                .getValue(bindings.get(variableIndex)) != null;
    }

    @Override
    public boolean isBound(String variableName) {
        Objects.requireNonNull(variableName);
        return bindings.contains(variableName) && current.getValue(variableName) != null;
    }

    @Override
    public boolean getBoolean(int columnIndex) throws OntoDriverException {
        ensureOpen();
        return toBoolean(getLiteralValue(columnIndex));
    }

    @Override
    public boolean getBoolean(String columnLabel) throws OntoDriverException {
        ensureOpen();
        return toBoolean(getLiteralValue(columnLabel));
    }

    private static boolean toBoolean(Object ob) {
        if (ob instanceof Boolean) {
            return (boolean) ob;
        } else {
            return Boolean.parseBoolean(ob.toString());
        }
    }

    @Override
    public byte getByte(int columnIndex) throws OntoDriverException {
        ensureOpen();
        return (byte) toInt(getLiteralValue(columnIndex));
    }

    @Override
    public byte getByte(String columnLabel) throws OntoDriverException {
        ensureOpen();
        return (byte) toInt(getLiteralValue(columnLabel));
    }

    @Override
    public double getDouble(int columnIndex) throws OntoDriverException {
        ensureOpen();
        return toDouble(getLiteralValue(columnIndex));
    }

    @Override
    public double getDouble(String columnLabel) throws OntoDriverException {
        ensureOpen();
        return toDouble(getLiteralValue(columnLabel));
    }

    private static double toDouble(Object ob) throws OntoDriverException {
        if (ob instanceof Number) {
            return ((Number) ob).doubleValue();
        } else {
            try {
                return Double.parseDouble(ob.toString());
            } catch (NumberFormatException e) {
                throw new OntoDriverException(e);
            }
        }
    }

    @Override
    public float getFloat(int columnIndex) throws OntoDriverException {
        ensureOpen();
        return toFloat(getLiteralValue(columnIndex));
    }

    @Override
    public float getFloat(String columnLabel) throws OntoDriverException {
        ensureOpen();
        return toFloat(getLiteralValue(columnLabel));
    }

    private static float toFloat(Object ob) throws OntoDriverException {
        if (ob instanceof Number) {
            return ((Number) ob).floatValue();
        } else {
            try {
                return Float.parseFloat(ob.toString());
            } catch (NumberFormatException e) {
                throw new OntoDriverException(e);
            }
        }
    }

    @Override
    public int getInt(int columnIndex) throws OntoDriverException {
        ensureOpen();
        return toInt(getLiteralValue(columnIndex));
    }

    @Override
    public int getInt(String columnLabel) throws OntoDriverException {
        ensureOpen();
        return toInt(getLiteralValue(columnLabel));
    }

    private static int toInt(Object ob) throws OntoDriverException {
        if (ob instanceof Number) {
            return ((Number) ob).intValue();
        } else {
            try {
                return Integer.parseInt(ob.toString());
            } catch (NumberFormatException e) {
                throw new OntoDriverException(e);
            }
        }
    }

    @Override
    public long getLong(int columnIndex) throws OntoDriverException {
        ensureOpen();
        return toLong(getLiteralValue(columnIndex));
    }

    @Override
    public long getLong(String columnLabel) throws OntoDriverException {
        ensureOpen();
        return toLong(getLiteralValue(columnLabel));
    }

    private static long toLong(Object ob) throws OntoDriverException {
        if (ob instanceof Number) {
            return ((Number) ob).longValue();
        } else {
            try {
                return Long.parseLong(ob.toString());
            } catch (NumberFormatException e) {
                throw new OntoDriverException(e);
            }
        }
    }

    @Override
    public Object getObject(int columnIndex) {
        ensureOpen();
        return toObject(getCurrent(columnIndex));
    }

    @Override
    public Object getObject(String columnLabel) {
        ensureOpen();
        return toObject(getCurrent(columnLabel));
    }

    private static Object toObject(Value val) {
        assert val != null;
        if (val instanceof Literal) {
            return Rdf4jUtils.getLiteralValue((Literal) val);
        } else if (val instanceof IRI) {
            return Rdf4jUtils.toJavaUri((IRI) val);
        } else {
            return val.toString();
        }
    }

    @Override
    public <T> T getObject(int columnIndex, Class<T> cls) throws OntoDriverException {
        ensureOpen();
        return toObject(getCurrent(columnIndex), cls);
    }

    @Override
    public <T> T getObject(String columnLabel, Class<T> cls) throws OntoDriverException {
        ensureOpen();
        return toObject(getCurrent(columnLabel), cls);
    }

    private static <T> T toObject(Value val, Class<T> cls) throws OntoDriverException {
        assert val != null;
        if (cls.isAssignableFrom(val.getClass())) {
            return cls.cast(val);
        }
        Object ob = null;
        if (val instanceof Literal) {
            ob = Rdf4jUtils.getLiteralValue((Literal) val);
        } else if (val instanceof IRI) {
            ob = Rdf4jUtils.toJavaUri((IRI) val);
        }
        try {
            return cls.cast(DatatypeTransformer.transform(ob, cls));
        } catch (UnsupportedTypeTransformationException e) {
            throw new Rdf4jDriverException("Unable to transform value to target object.", e);
        }
    }

    @Override
    public short getShort(int columnIndex) throws OntoDriverException {
        ensureOpen();
        return (short) toInt(getLiteralValue(columnIndex));
    }

    @Override
    public short getShort(String columnLabel) throws OntoDriverException {
        ensureOpen();
        return (short) toInt(getLiteralValue(columnLabel));
    }

    @Override
    public String getString(int columnIndex) {
        ensureOpen();
        return getStringImpl(getCurrent(columnIndex));
    }

    @Override
    public String getString(String columnLabel) {
        ensureOpen();
        return getStringImpl(getCurrent(columnLabel));
    }

    private static String getStringImpl(Value val) {
        if (val instanceof Literal) {
            return Rdf4jUtils.getLiteralValue((Literal) val).toString();
        } else {
            return val.toString();
        }
    }

    @Override
    public boolean hasNext() throws OntoDriverException {
        ensureOpen();
        try {
            return result.hasNext();
        } catch (QueryEvaluationException e) {
            throw new OntoDriverException(e);
        }
    }

    @Override
    public void next() throws OntoDriverException {
        super.next();
        try {
            this.current = result.next();
        } catch (QueryEvaluationException e) {
            throw new OntoDriverException(e);
        }
    }

    private Object getLiteralValue(int columnIndex) throws OntoDriverException {
        final Value val = getCurrent(columnIndex);
        if (!(val instanceof Literal)) {
            throw new OntoDriverException("Expected value " + val + " to be a literal.");
        }
        return Rdf4jUtils.getLiteralValue((Literal) val);
    }

    private Object getLiteralValue(String columnName) throws OntoDriverException {
        final Value val = getCurrent(columnName);
        if (!(val instanceof Literal)) {
            throw new OntoDriverException("Expected value " + val + " to be a literal.");
        }
        return Rdf4jUtils.getLiteralValue((Literal) val);
    }

    private Value getCurrent(int columnIndex) {
        ensureState();
        if (columnIndex < 0 || columnIndex >= bindings.size()) {
            throw new IllegalArgumentException(
                    "The column index is out of bounds of the column count.");
        }
        final Value v = current.getValue(bindings.get(columnIndex));
        if (v == null) {
            throw new VariableNotBoundException(
                    "Variable at index " + columnIndex + " is not bound in the current result row.");
        }
        return v;
    }

    private void ensureState() {
        if (current == null) {
            throw new IllegalStateException("Must call next before getting the first value.");
        }
    }

    private Value getCurrent(String columnName) {
        ensureState();
        if (!bindings.contains(columnName)) {
            throw new IllegalArgumentException("Unknown column name " + columnName);
        }
        final Value v = current.getValue(columnName);
        if (v == null) {
            throw new VariableNotBoundException(
                    "Variable \"" + columnName + "\" is not bound in the current result row.");
        }
        return v;
    }
}

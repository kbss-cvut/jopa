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
package cz.cvut.kbss.ontodriver.sesame.query;

import cz.cvut.kbss.ontodriver.Statement;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;
import cz.cvut.kbss.ontodriver.sesame.util.SesameUtils;
import org.eclipse.rdf4j.model.IRI;
import org.eclipse.rdf4j.model.Literal;
import org.eclipse.rdf4j.model.Value;
import org.eclipse.rdf4j.query.BindingSet;
import org.eclipse.rdf4j.query.QueryEvaluationException;
import org.eclipse.rdf4j.query.TupleQueryResult;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.List;

// TODO Resolve mapping of data values with language tag
public class SelectResultSet extends AbstractResultSet {

    private final TupleQueryResult result;
    private List<String> bindings;
    private BindingSet current;

    public SelectResultSet(TupleQueryResult result, Statement statement) throws QueryEvaluationException {
        super(statement);
        assert result != null;

        this.result = result;
        init();
    }

    private void init() throws QueryEvaluationException {
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
    public boolean getBoolean(int columnIndex) throws OntoDriverException {
        ensureOpen();
        return toBoolean(getLiteralValue(columnIndex));
    }

    @Override
    public boolean getBoolean(String columnLabel) throws OntoDriverException {
        ensureOpen();
        return toBoolean(getLiteralValue(columnLabel));
    }

    private boolean toBoolean(Object ob) {
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

    private double toDouble(Object ob) throws OntoDriverException {
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

    private float toFloat(Object ob) throws OntoDriverException {
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

    private int toInt(Object ob) throws OntoDriverException {
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

    private long toLong(Object ob) throws OntoDriverException {
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
    public Object getObject(int columnIndex) throws OntoDriverException {
        ensureOpen();
        return toObject(getCurrent(columnIndex));
    }

    @Override
    public Object getObject(String columnLabel) throws OntoDriverException {
        ensureOpen();
        return toObject(getCurrent(columnLabel));
    }

    private Object toObject(Value val) {
        if (val == null) {
            return null;
        }
        if (val instanceof Literal) {
            return SesameUtils.getDataPropertyValue((Literal) val, null).get();
        } else if (val instanceof IRI) {
            return SesameUtils.toJavaUri((IRI) val);
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

    private <T> T toObject(Value val, Class<T> cls) throws OntoDriverException {
        if (val == null) {
            return null;
        }
        if (cls.isAssignableFrom(val.getClass())) {
            return cls.cast(val);
        }
        Object ob = null;
        if (val instanceof Literal) {
            ob = SesameUtils.getDataPropertyValue((Literal) val, null).get();
        } else if (val instanceof IRI) {
            ob = SesameUtils.toJavaUri((IRI) val);
        }
        if (ob != null && cls.isAssignableFrom(ob.getClass())) {
            return cls.cast(ob);
        }
        ob = val;
        return instantiateUsingConstructor(cls, val, ob);
    }

    /**
     * Searches for a suitable constructor and creates a new instance of class {@code cls}. </p>
     * <p>
     * The type has to have single-argument constructor, which takes either {@code Value} or its subtypes or type of
     * instance returned by {@link SesameUtils#getDataPropertyValue(Literal, String)}.
     *
     * @param cls The return type
     * @param val Raw value
     * @param ob  Either raw value (if it is a resource) or instance returned by {@link
     *            SesameUtils#getDataPropertyValue(Literal, String)} on passing the literal {@code val}
     * @return The new instance
     * @throws OntoDriverException If no suitable constructor is found or the instance cannot be created
     */
    private <T> T instantiateUsingConstructor(Class<T> cls, Value val, Object ob)
            throws OntoDriverException {
        Constructor<?>[] ctors = cls.getDeclaredConstructors();
        try {
            for (Constructor<?> c : ctors) {
                if (c.getParameterTypes().length != 1) {
                    continue;
                }
                c.setAccessible(true);
                final Class<?> type = c.getParameterTypes()[0];
                if (type.isAssignableFrom(ob.getClass())) {
                    return (T) c.newInstance(ob);
                }
                if (type.isAssignableFrom(val.getClass())) {
                    return (T) c.newInstance(val);
                }
                if (type.isAssignableFrom(String.class)) {
                    return (T) c.newInstance(ob.toString());
                }
            }
        } catch (InstantiationException | IllegalAccessException | IllegalArgumentException
                | InvocationTargetException e) {
            throw new SesameDriverException("Unable to create instance of type " + cls + " with value " + val, e);
        }
        throw new SesameDriverException("No suitable constructor for value " + val + " found in type " + cls);
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
    public String getString(int columnIndex) throws OntoDriverException {
        ensureOpen();
        return getStringImpl(getCurrent(columnIndex));
    }

    @Override
    public String getString(String columnLabel) throws OntoDriverException {
        ensureOpen();
        return getStringImpl(getCurrent(columnLabel));
    }

    private String getStringImpl(Value val) {
        if (val instanceof Literal) {
            return SesameUtils.getDataPropertyValue((Literal) val, null).get().toString();
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
        return SesameUtils.getDataPropertyValue((Literal) val, null).get();
    }

    private Object getLiteralValue(String columnName) throws OntoDriverException {
        final Value val = getCurrent(columnName);
        if (!(val instanceof Literal)) {
            throw new OntoDriverException("Expected value " + val + " to be a literal.");
        }
        return SesameUtils.getDataPropertyValue((Literal) val, null).get();
    }

    private Value getCurrent(int columnIndex) {
        ensureState();
        if (columnIndex < 0 || columnIndex >= bindings.size()) {
            throw new IllegalArgumentException(
                    "The column index is out of bounds of the column count.");
        }
        return current.getValue(bindings.get(columnIndex));
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
        return current.getValue(columnName);
    }
}

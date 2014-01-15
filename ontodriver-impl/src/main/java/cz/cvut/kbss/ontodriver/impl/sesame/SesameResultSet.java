package cz.cvut.kbss.ontodriver.impl.sesame;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.List;
import java.util.Observer;

import org.openrdf.model.Literal;
import org.openrdf.model.Value;
import org.openrdf.query.BindingSet;
import org.openrdf.query.QueryEvaluationException;
import org.openrdf.query.TupleQueryResult;
import org.openrdf.repository.RepositoryConnection;
import org.openrdf.repository.RepositoryException;

import cz.cvut.kbss.ontodriver.AbstractResultSet;
import cz.cvut.kbss.ontodriver.Statement;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;

public class SesameResultSet extends AbstractResultSet {

	private final RepositoryConnection connection;
	private final TupleQueryResult result;
	private List<String> bindings;
	private BindingSet current;

	public SesameResultSet(RepositoryConnection connection, TupleQueryResult result,
			Statement statement) throws QueryEvaluationException {
		super(statement);
		if (connection == null || result == null) {
			throw new NullPointerException();
		}
		this.connection = connection;
		this.result = result;
		init();
	}

	private void init() throws QueryEvaluationException {
		this.bindings = result.getBindingNames();
	}

	@Override
	public void close() throws OntoDriverException {
		super.close();
		try {
			connection.close();
		} catch (RepositoryException e) {
			throw new OntoDriverException(e);
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
	public void first() throws OntoDriverException {
		throw new UnsupportedOperationException(
				"Returning to the first row is not supported by this result set.");
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
		if (val instanceof Literal) {
			return SesameUtils.getDataPropertyValue((Literal) val);
		} else {
			return val;
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
		if (cls.isAssignableFrom(val.getClass())) {
			return cls.cast(val);
		}
		Object ob = val;
		if (val instanceof Literal) {
			ob = SesameUtils.getDataPropertyValue((Literal) val);
			if (cls.isAssignableFrom(ob.getClass())) {
				return cls.cast(ob);
			}
		}
		return instantiateUsingConstructor(cls, val, ob);
	}

	/**
	 * Searches for a suitable constructor and creates a new instance of class
	 * {@code cls}. </p>
	 * 
	 * The type has to have single-argument constructor, which takes either
	 * {@code Value} or its subtypes or type of instance returned by
	 * {@link SesameUtils#getDataPropertyValue(Literal)}.
	 * 
	 * @param cls
	 *            The return type
	 * @param val
	 *            Raw value
	 * @param ob
	 *            Either raw value (if it is a resource) or instance returned by
	 *            {@link SesameUtils#getDataPropertyValue(Literal)} on passing
	 *            the literal {@code val}
	 * @return The new instance
	 * @throws OntoDriverException
	 *             If no suitable constructor is found or the instance cannot be
	 *             created
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
			throw new OntoDriverException("Unable to create instance of type " + cls
					+ " with value " + val, e);
		}
		throw new OntoDriverException("No suitable constructor for value " + val
				+ " found in type " + cls);
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
			return SesameUtils.getDataPropertyValue((Literal) val).toString();
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
	public void last() throws OntoDriverException {
		ensureOpen();
		while (hasNext()) {
			next();
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

	@Override
	public void previous() throws OntoDriverException {
		throw new UnsupportedOperationException("Going back is not supported by this result set.");
	}

	@Override
	public void registerObserver(Observer observer) throws OntoDriverException {
		// TODO Auto-generated method stub

	}

	@Override
	public void relative(int rows) throws OntoDriverException {
		setRowIndex(index + rows);
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

	private Object getLiteralValue(int columnIndex) throws OntoDriverException {
		final Value val = getCurrent(columnIndex);
		if (!(val instanceof Literal)) {
			throw new OntoDriverException("Expected value " + val + " to be a literal.");
		}
		return SesameUtils.getDataPropertyValue((Literal) val);
	}

	private Object getLiteralValue(String columnName) throws OntoDriverException {
		final Value val = getCurrent(columnName);
		if (!(val instanceof Literal)) {
			throw new OntoDriverException("Expected value " + val + " to be a literal.");
		}
		return SesameUtils.getDataPropertyValue((Literal) val);
	}

	private Value getCurrent(int columnIndex) {
		if (columnIndex < 0 || columnIndex >= bindings.size()) {
			throw new IllegalArgumentException(
					"The column index is out of bounds of the column count.");
		}
		return current.getValue(bindings.get(columnIndex));
	}

	private Value getCurrent(String columnName) {
		if (!bindings.contains(columnName)) {
			throw new IllegalArgumentException("Unknown column name " + columnName);
		}
		return current.getValue(columnName);
	}
}

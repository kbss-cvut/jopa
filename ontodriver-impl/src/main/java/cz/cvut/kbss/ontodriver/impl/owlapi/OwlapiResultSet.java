package cz.cvut.kbss.ontodriver.impl.owlapi;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Observer;

import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLLiteral;
import org.semanticweb.owlapi.model.OWLObject;

import cz.cvut.kbss.jopa.owlapi.DatatypeTransformer;
import cz.cvut.kbss.ontodriver.AbstractResultSet;
import cz.cvut.kbss.ontodriver.Statement;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.owl2query.model.GroundTerm;
import cz.cvut.kbss.owl2query.model.QueryResult;
import cz.cvut.kbss.owl2query.model.ResultBinding;
import cz.cvut.kbss.owl2query.model.Variable;

public class OwlapiResultSet extends AbstractResultSet {

	private final QueryResult<OWLObject> result;
	private Iterator<ResultBinding<OWLObject>> iterator;
	private ResultBinding<OWLObject> current;
	private final Map<String, Variable<OWLObject>> namesToVars;
	private final Map<Integer, Variable<OWLObject>> indexesToVars;

	public OwlapiResultSet(QueryResult<OWLObject> result, Statement statement) {
		super(statement);
		if (result == null) {
			throw new NullPointerException();
		}
		this.result = result;
		this.namesToVars = new HashMap<String, Variable<OWLObject>>(result.getResultVars().size());
		this.indexesToVars = new HashMap<Integer, Variable<OWLObject>>(result.getResultVars()
				.size());
		init();
	}

	private void init() {
		Integer i = 0;
		for (Variable<OWLObject> v : result.getResultVars()) {
			namesToVars.put(v.getName(), v);
			indexesToVars.put(i, v);
			i++;
		}
		this.iterator = result.iterator();
	}

	@Override
	public int findColumn(String columnLabel) {
		ensureOpen();
		for (Entry<Integer, Variable<OWLObject>> e : indexesToVars.entrySet()) {
			if (e.getValue().getName().equals(columnLabel)) {
				return e.getKey();
			}
		}
		return -1;
	}

	@Override
	public int getColumnCount() {
		ensureOpen();
		return result.getResultVars().size();
	}

	@Override
	public void first() throws OntoDriverException {
		ensureOpen();
		this.iterator = result.iterator();
		next();
	}

	@Override
	public boolean getBoolean(int columnIndex) throws OntoDriverException {
		final String s = getString(columnIndex);
		return Boolean.parseBoolean(s);
	}

	@Override
	public boolean getBoolean(String columnLabel) throws OntoDriverException {
		final String s = getString(columnLabel);
		return Boolean.parseBoolean(s);
	}

	@Override
	public byte getByte(int columnIndex) throws OntoDriverException {
		final String s = getString(columnIndex);
		return Byte.parseByte(s);
	}

	@Override
	public byte getByte(String columnLabel) throws OntoDriverException {
		final String s = getString(columnLabel);
		return Byte.parseByte(s);
	}

	@Override
	public double getDouble(int columnIndex) throws OntoDriverException {
		final String s = getString(columnIndex);
		return Double.parseDouble(s);
	}

	@Override
	public double getDouble(String columnLabel) throws OntoDriverException {
		final String s = getString(columnLabel);
		return Double.parseDouble(s);
	}

	@Override
	public float getFloat(int columnIndex) throws OntoDriverException {
		final String s = getString(columnIndex);
		return Float.parseFloat(s);
	}

	@Override
	public float getFloat(String columnLabel) throws OntoDriverException {
		final String s = getString(columnLabel);
		return Float.parseFloat(s);
	}

	@Override
	public int getInt(int columnIndex) throws OntoDriverException {
		final String s = getString(columnIndex);
		return Integer.parseInt(s);
	}

	@Override
	public int getInt(String columnLabel) throws OntoDriverException {
		final String s = getString(columnLabel);
		return Integer.parseInt(s);
	}

	@Override
	public long getLong(int columnIndex) throws OntoDriverException {
		final String s = getString(columnIndex);
		return Long.parseLong(s);
	}

	@Override
	public long getLong(String columnLabel) throws OntoDriverException {
		final String s = getString(columnLabel);
		return Long.parseLong(s);
	}

	@Override
	public Object getObject(int columnIndex) throws OntoDriverException {
		return getCurrentValue(columnIndex);
	}

	@Override
	public Object getObject(String columnLabel) throws OntoDriverException {
		return getCurrentValue(columnLabel);
	}

	@Override
	public <T> T getObject(int columnIndex, Class<T> cls) throws OntoDriverException {
		final OWLObject ob = getCurrentValue(columnIndex);
		if (cls == null) {
			throw new NullPointerException();
		}
		return getObjectImpl(ob, cls);
	}

	@Override
	public <T> T getObject(String columnLabel, Class<T> cls) throws OntoDriverException {
		final OWLObject ob = getCurrentValue(columnLabel);
		if (cls == null) {
			throw new NullPointerException();
		}
		return getObjectImpl(ob, cls);
	}

	@Override
	public short getShort(int columnIndex) throws OntoDriverException {
		final String s = getString(columnIndex);
		return Short.parseShort(s);
	}

	@Override
	public short getShort(String columnLabel) throws OntoDriverException {
		final String s = getString(columnLabel);
		return Short.parseShort(s);
	}

	@Override
	public String getString(int columnIndex) throws OntoDriverException {
		final OWLObject ob = getCurrentValue(columnIndex);
		return getString(ob);
	}

	@Override
	public String getString(String columnLabel) throws OntoDriverException {
		final OWLObject ob = getCurrentValue(columnLabel);
		return getString(ob);
	}

	@Override
	public boolean hasNext() throws OntoDriverException {
		ensureOpen();
		return iterator.hasNext();
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
		current = iterator.next();
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

	private OWLObject getCurrentValue(String column) {
		ensureOpen();
		if (current == null) {
			throw new IllegalStateException("Current row is null.");
		}
		final Variable<OWLObject> v = namesToVars.get(column);
		if (v == null) {
			throw new IllegalArgumentException("Unknown column name " + column);
		}
		final GroundTerm<OWLObject> gt = current.get(v);
		assert v != null;
		return gt.getWrappedObject();
	}

	private OWLObject getCurrentValue(int colIndex) {
		ensureOpen();
		if (current == null) {
			throw new IllegalStateException("Current row is null.");
		}
		final Variable<OWLObject> v = indexesToVars.get(colIndex);
		if (v == null) {
			throw new IllegalArgumentException("Column index out of bounds: " + colIndex);
		}
		final GroundTerm<OWLObject> gt = current.get(v);
		assert v != null;
		return gt.getWrappedObject();
	}

	private String getString(OWLObject ob) {
		if (ob instanceof OWLLiteral) {
			return (DatatypeTransformer.transform((OWLLiteral) ob).toString());
		} else if (ob instanceof OWLEntity) {
			return ((OWLEntity) ob).getIRI().toString();
		} else {
			return ob.toString();
		}
	}

	private <T> T getObjectImpl(OWLObject ob, Class<T> cls) throws OntoDriverException {
		if (cls.isAssignableFrom(ob.getClass())) {
			return cls.cast(ob);
		} else {
			Object lit = (ob instanceof OWLLiteral) ? DatatypeTransformer
					.transform((OWLLiteral) ob) : null;
			if (lit != null && cls.isAssignableFrom(lit.getClass())) {
				return cls.cast(lit);
			}
			OWLEntity ent = (ob instanceof OWLEntity) ? (OWLEntity) ob : null;
			try {
				final Constructor<?>[] ctors = cls.getDeclaredConstructors();
				for (Constructor<?> c : ctors) {
					if (c.getParameterTypes().length != 1) {
						continue;
					}
					c.setAccessible(true);
					final Class<?> type = c.getParameterTypes()[0];
					if (lit != null && type.isAssignableFrom(lit.getClass())) {
						// Constructor takes literal value
						return (T) c.newInstance(lit);
					}
					if (ent != null && type.isAssignableFrom(ent.getClass())) {
						// Constructor takes OWLEntity sub-class
						return (T) c.newInstance(ent);
					}
					if (ent != null && type.isAssignableFrom(IRI.class)) {
						// Constructor takes IRI
						return (T) c.newInstance(ent.getIRI());
					}
					if (type.isAssignableFrom(ob.getClass())) {
						// Constructor takes OWLObject sub-class
						return (T) c.newInstance(ob);
					}
					if (type.isAssignableFrom(String.class)) {
						// Fallback, constructor taking String
						if (lit != null) {
							return (T) c.newInstance(lit.toString());
						} else if (ent != null) {
							return (T) c.newInstance(ent.getIRI().toString());
						} else {
							return (T) c.newInstance(ob.toString());
						}
					}
				}
				final Constructor<T> c = cls.getDeclaredConstructor(cls);
				c.setAccessible(true);
				return c.newInstance(ob);
			} catch (NoSuchMethodException e) {
				throw new OntoDriverException(
						"Unable to find a costructor taking OWLObject argument in class "
								+ cls.getCanonicalName());
			} catch (InstantiationException | IllegalAccessException | IllegalArgumentException
					| InvocationTargetException e) {
				throw new OntoDriverException("Unable to instantiate object of class "
						+ cls.getCanonicalName() + " with argument " + ob);
			}
		}
	}
}

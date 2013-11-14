package cz.cvut.kbss.ontodriver.impl.owlapi;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Observer;

import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLLiteral;
import org.semanticweb.owlapi.model.OWLObject;

import cz.cvut.kbss.jopa.owlapi.DatatypeTransformer;
import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.Statement;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.owl2query.model.GroundTerm;
import cz.cvut.kbss.owl2query.model.QueryResult;
import cz.cvut.kbss.owl2query.model.ResultBinding;
import cz.cvut.kbss.owl2query.model.Variable;

public class OwlapiResultSet implements ResultSet {

	private final QueryResult<OWLObject> result;
	private Iterator<ResultBinding<OWLObject>> iterator;
	private ResultBinding<OWLObject> current;
	private final Map<String, Variable<OWLObject>> namesToVars;
	private final Map<Integer, Variable<OWLObject>> indexesToVars;
	private final Statement statement;
	private boolean open;

	public OwlapiResultSet(QueryResult<OWLObject> result, Statement statement) {
		if (result == null || statement == null) {
			throw new NullPointerException();
		}
		this.result = result;
		this.statement = statement;
		this.namesToVars = new HashMap<String, Variable<OWLObject>>(result.getResultVars().size());
		this.indexesToVars = new HashMap<Integer, Variable<OWLObject>>(result.getResultVars()
				.size());
		init();
		this.open = true;
	}

	private void init() {
		Integer i = 0;
		for (Variable<OWLObject> v : result.getResultVars()) {
			namesToVars.put(v.getName(), v);
			indexesToVars.put(i, v);
			i++;
		}
		this.iterator = result.iterator();
		this.current = iterator.hasNext() ? iterator.next() : null;
	}

	@Override
	public boolean isOpen() {
		return open;
	}

	@Override
	public void close() throws OntoDriverException {
		this.open = false;
	}

	@Override
	public int findColumn(String columnLabel) throws OntoDriverException {
		for (Entry<Integer, Variable<OWLObject>> e : indexesToVars.entrySet()) {
			if (e.getValue().getName().equals(columnLabel)) {
				return e.getKey();
			}
		}
		return -1;
	}

	@Override
	public int getColumnCount() throws OntoDriverException {
		return result.getResultVars().size();
	}

	@Override
	public void first() throws OntoDriverException {
		this.iterator = result.iterator();
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
		if (cls == null) {
			throw new NullPointerException();
		}
		final OWLObject ob = getCurrentValue(columnIndex);
		if (cls.isAssignableFrom(ob.getClass())) {
			return (T) ob;
		} else {
			// TODO
			return null;
		}
	}

	@Override
	public <T> T getObject(String columnLabel, Class<T> cls) throws OntoDriverException {
		if (cls == null) {
			throw new NullPointerException();
		}
		final OWLObject ob = getCurrentValue(columnLabel);
		if (cls.isAssignableFrom(ob.getClass())) {
			return (T) ob;
		} else {
			// TODO
			return null;
		}
	}

	@Override
	public int getRowIndex() throws OntoDriverException {
		// TODO Auto-generated method stub
		return 0;
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
	public Statement getStatement() throws OntoDriverException {
		return statement;
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
	public boolean isFirst() throws OntoDriverException {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public boolean hasNext() throws OntoDriverException {
		return iterator.hasNext();
	}

	@Override
	public void last() throws OntoDriverException {
		// TODO Auto-generated method stub

	}

	@Override
	public void next() throws OntoDriverException {
		current = iterator.next();
	}

	@Override
	public void previous() throws OntoDriverException {
		// TODO Auto-generated method stub

	}

	@Override
	public void registerObserver(Observer observer) throws OntoDriverException {
		// TODO Auto-generated method stub

	}

	@Override
	public void relative(int rows) throws OntoDriverException {
		// TODO Auto-generated method stub

	}

	@Override
	public void setRowIndex(int rowIndex) throws OntoDriverException {
		// TODO Auto-generated method stub

	}

	private OWLObject getCurrentValue(String column) {
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
}

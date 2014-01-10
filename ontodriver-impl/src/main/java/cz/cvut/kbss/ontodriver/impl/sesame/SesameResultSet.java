package cz.cvut.kbss.ontodriver.impl.sesame;

import java.util.List;
import java.util.Observer;

import org.openrdf.query.QueryEvaluationException;
import org.openrdf.query.TupleQueryResult;

import cz.cvut.kbss.ontodriver.AbstractResultSet;
import cz.cvut.kbss.ontodriver.Statement;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;

public class SesameResultSet extends AbstractResultSet {

	private final TupleQueryResult result;
	private List<String> bindings;

	public SesameResultSet(TupleQueryResult result, Statement statement)
			throws QueryEvaluationException {
		super(statement);
		if (result == null) {
			throw new NullPointerException();
		}
		this.result = result;
		init();
	}

	private void init() throws QueryEvaluationException {
		this.bindings = result.getBindingNames();
	}

	@Override
	public int findColumn(String columnLabel) throws OntoDriverException {
		ensureOpen();
		return bindings.indexOf(columnLabel);
	}

	@Override
	public int getColumnCount() throws OntoDriverException {
		ensureOpen();
		return bindings.size();
	}

	@Override
	public void first() throws OntoDriverException {
		// TODO Auto-generated method stub

	}

	@Override
	public boolean getBoolean(int columnIndex) throws OntoDriverException {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public boolean getBoolean(String columnLabel) throws OntoDriverException {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public byte getByte(int columnIndex) throws OntoDriverException {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public byte getByte(String columnLabel) throws OntoDriverException {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public double getDouble(int columnIndex) throws OntoDriverException {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public double getDouble(String columnLabel) throws OntoDriverException {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public float getFloat(int columnIndex) throws OntoDriverException {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public float getFloat(String columnLabel) throws OntoDriverException {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public int getInt(int columnIndex) throws OntoDriverException {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public int getInt(String columnLabel) throws OntoDriverException {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public long getLong(int columnIndex) throws OntoDriverException {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public long getLong(String columnLabel) throws OntoDriverException {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public Object getObject(int columnIndex) throws OntoDriverException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Object getObject(String columnLabel) throws OntoDriverException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public <T> T getObject(int columnIndex, Class<T> cls) throws OntoDriverException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public <T> T getObject(String columnLabel, Class<T> cls) throws OntoDriverException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public short getShort(int columnIndex) throws OntoDriverException {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public short getShort(String columnLabel) throws OntoDriverException {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public String getString(int columnIndex) throws OntoDriverException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String getString(String columnLabel) throws OntoDriverException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public boolean hasNext() throws OntoDriverException {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public void last() throws OntoDriverException {
		// TODO Auto-generated method stub

	}

	@Override
	public void next() throws OntoDriverException {
		super.next();
		// TODO Auto-generated method stub
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
}

package cz.cvut.kbss.ontodriver.impl;

import cz.cvut.kbss.ontodriver.Context;
import cz.cvut.kbss.ontodriver.OntoDriverException;
import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.Statement;
import cz.cvut.kbss.ontodriver.StorageModule;

public class OwlapiStorageModule extends StorageModule {

	public OwlapiStorageModule(Context context) {
		super(context);
		// TODO Auto-generated constructor stub
	}

	@Override
	public void commit() throws OntoDriverException {
		// TODO Auto-generated method stub

	}

	@Override
	public void rollback() throws OntoDriverException {
		// TODO Auto-generated method stub

	}

	@Override
	protected void initialize() {
		// TODO Auto-generated method stub

	}

	@Override
	public <T> T find(Class<T> cls, Object primaryKey)
			throws OntoDriverException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public <T> void merge(Object primaryKey, T entity)
			throws OntoDriverException {
		// TODO Auto-generated method stub

	}

	@Override
	public <T> void persist(Object primaryKey, T entity)
			throws OntoDriverException {
		// TODO Auto-generated method stub

	}

	@Override
	public void remove(Object primaryKey) throws OntoDriverException {
		// TODO Auto-generated method stub

	}

	@Override
	public ResultSet executeStatement(Statement statement)
			throws OntoDriverException {
		// TODO Auto-generated method stub
		return null;
	}

}

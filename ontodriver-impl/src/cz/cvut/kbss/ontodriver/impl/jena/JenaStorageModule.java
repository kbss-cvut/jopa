package cz.cvut.kbss.ontodriver.impl.jena;

import java.lang.reflect.Field;

import cz.cvut.kbss.ontodriver.Context;
import cz.cvut.kbss.ontodriver.DriverFactory;
import cz.cvut.kbss.ontodriver.PersistenceProviderFacade;
import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.Statement;
import cz.cvut.kbss.ontodriver.StorageModule;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;

public class JenaStorageModule extends StorageModule {

	public JenaStorageModule(Context context, PersistenceProviderFacade persistenceProvider,
			DriverFactory factory) throws OntoDriverException {
		super(context, persistenceProvider, factory);
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
	public boolean contains(Object primaryKey) throws OntoDriverException {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public <T> T find(Class<T> cls, Object primaryKey) throws OntoDriverException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public <T> void loadFieldValue(T entity, Field field) throws OntoDriverException {
		// TODO Auto-generated method stub

	}

	@Override
	public <T> void merge(Object primaryKey, T entity) throws OntoDriverException {
		// TODO Auto-generated method stub

	}

	@Override
	public <T> void persist(Object primaryKey, T entity) throws OntoDriverException {
		// TODO Auto-generated method stub

	}

	@Override
	public void remove(Object primaryKey) throws OntoDriverException {
		// TODO Auto-generated method stub

	}

	@Override
	public ResultSet executeStatement(Statement statement) throws OntoDriverException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	protected void startTransactionIfNotActive() throws OntoDriverException {
		// TODO Auto-generated method stub
		
	}
}

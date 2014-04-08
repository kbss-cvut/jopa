package cz.cvut.kbss.ontodriver.impl.jena;

import java.lang.reflect.Field;

import cz.cvut.kbss.jopa.model.Repository;
import cz.cvut.kbss.jopa.model.RepositoryID;
import cz.cvut.kbss.ontodriver.DriverFactory;
import cz.cvut.kbss.ontodriver.JopaStatement;
import cz.cvut.kbss.ontodriver.PersistenceProviderFacade;
import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.StorageModule;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;

public class JenaStorageModule extends StorageModule {

	public JenaStorageModule(Repository repository, PersistenceProviderFacade persistenceProvider,
			DriverFactory factory) throws OntoDriverException {
		super(repository, persistenceProvider, factory);
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
	protected void initialize() throws OntoDriverException {
		// TODO Auto-generated method stub

	}

	@Override
	public boolean contains(Object primaryKey, RepositoryID contexts) throws OntoDriverException {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public <T> T find(Class<T> cls, Object primaryKey, RepositoryID contexts)
			throws OntoDriverException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public boolean isConsistent(RepositoryID contexts) throws OntoDriverException {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public <T> void loadFieldValue(T entity, Field field, RepositoryID contexts)
			throws OntoDriverException {
		// TODO Auto-generated method stub

	}

	@Override
	public <T> void merge(T entity, Field mergedField, RepositoryID context)
			throws OntoDriverException {
		// TODO Auto-generated method stub

	}

	@Override
	public <T> void persist(Object primaryKey, T entity, RepositoryID context)
			throws OntoDriverException {
		// TODO Auto-generated method stub

	}

	@Override
	public void remove(Object primaryKey, RepositoryID context) throws OntoDriverException {
		// TODO Auto-generated method stub

	}

	@Override
	public ResultSet executeStatement(JopaStatement statement) throws OntoDriverException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	protected void startTransactionIfNotActive() throws OntoDriverException {
		// TODO Auto-generated method stub

	}

}

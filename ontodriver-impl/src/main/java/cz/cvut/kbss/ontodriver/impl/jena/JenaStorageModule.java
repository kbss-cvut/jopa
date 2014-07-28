package cz.cvut.kbss.ontodriver.impl.jena;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.Collections;
import java.util.List;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.ontodriver.DriverFactory;
import cz.cvut.kbss.ontodriver.JopaStatement;
import cz.cvut.kbss.ontodriver.PersistenceProviderFacade;
import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.StorageModule;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;

public class JenaStorageModule extends StorageModule {

	public JenaStorageModule(PersistenceProviderFacade persistenceProvider, DriverFactory factory)
			throws OntoDriverException {
		super(persistenceProvider, factory);
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
	public boolean contains(Object primaryKey, URI context) throws OntoDriverException {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public <T> T find(Class<T> cls, Object primaryKey, Descriptor descriptor)
			throws OntoDriverException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public List<URI> getContexts() {
		return Collections.emptyList();
	}

	@Override
	public boolean isConsistent(URI context) throws OntoDriverException {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public <T> void loadFieldValue(T entity, Field field, Descriptor descriptor)
			throws OntoDriverException {
		// TODO Auto-generated method stub

	}

	@Override
	public <T> void merge(T entity, Field mergedField, Descriptor descriptor)
			throws OntoDriverException {
		// TODO Auto-generated method stub

	}

	@Override
	public <T> void persist(Object primaryKey, T entity, Descriptor descriptor)
			throws OntoDriverException {
		// TODO Auto-generated method stub

	}

	@Override
	public void remove(Object primaryKey, Descriptor descriptor) throws OntoDriverException {
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

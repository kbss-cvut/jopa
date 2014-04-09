package cz.cvut.kbss.ontodriver.utils;

import java.util.Collections;
import java.util.List;
import java.util.Map;

import cz.cvut.kbss.jopa.model.Repository;
import cz.cvut.kbss.jopa.model.EntityDescriptor;
import cz.cvut.kbss.ontodriver.DriverFactory;
import cz.cvut.kbss.ontodriver.DriverStatement;
import cz.cvut.kbss.ontodriver.JopaStatement;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.PersistenceProviderFacade;
import cz.cvut.kbss.ontodriver.StorageConnector;
import cz.cvut.kbss.ontodriver.StorageModule;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;

public class DriverFactoryStub implements DriverFactory {

	public static DriverFactoryStub instance;

	private boolean open = true;

	public DriverFactoryStub() {
	}

	public DriverFactoryStub(List<Repository> repositories,
			Map<EntityDescriptor, OntologyStorageProperties> repoProperties,
			Map<String, String> properties) {
		super();
		// This is not best practice, but we need to access the instance
		instance = this;
	}

	@Override
	public void close() throws OntoDriverException {
		open = false;
	}

	@Override
	public boolean isOpen() {
		return open;
	}

	@Override
	public void releaseStorageConnector(StorageConnector connector) throws OntoDriverException {
		// Do nothing
	}

	@Override
	public DriverStatement createStatement(JopaStatement statement) throws OntoDriverException {
		return null;
	}

	@Override
	public List<Repository> getRepositories() {
		return Collections.emptyList();
	}

	@Override
	public StorageModule createStorageModule(EntityDescriptor repository,
			PersistenceProviderFacade persistenceProvider, boolean autoCommit)
			throws OntoDriverException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void releaseStorageModule(StorageModule module) throws OntoDriverException {
		// Do nothing
	}

	@Override
	public StorageConnector createStorageConnector(EntityDescriptor repository, boolean autoCommit)
			throws OntoDriverException {
		// TODO Auto-generated method stub
		return null;
	}
}

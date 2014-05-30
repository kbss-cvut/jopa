package cz.cvut.kbss.ontodriver.utils;

import java.util.Map;

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

	private StorageModule storageModule;
	private StorageConnector storageConnector;

	public DriverFactoryStub() {
	}

	public DriverFactoryStub(OntologyStorageProperties repoProperties,
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
	public StorageModule createStorageModule(PersistenceProviderFacade persistenceProvider,
			boolean autoCommit) throws OntoDriverException {
		return storageModule;
	}

	public void setStorageModule(StorageModule module) {
		this.storageModule = module;
	}

	@Override
	public void releaseStorageModule(StorageModule module) throws OntoDriverException {
		// Do nothing
	}

	@Override
	public StorageConnector createStorageConnector(boolean autoCommit) throws OntoDriverException {
		return storageConnector;
	}

	public void setStorageConnector(StorageConnector connector) {
		this.storageConnector = connector;
	}
}

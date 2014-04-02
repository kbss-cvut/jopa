package cz.cvut.kbss.ontodriver.utils;

import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.ontodriver.Context;
import cz.cvut.kbss.ontodriver.DriverFactory;
import cz.cvut.kbss.ontodriver.PersistenceProviderFacade;
import cz.cvut.kbss.ontodriver.StorageManager;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver.impl.OntoDriverImpl;

public class OntoDriverMock extends OntoDriverImpl {

	private DriverFactoryStub factoryMock;

	public OntoDriverMock() {
		super();
		this.factoryMock = new DriverFactoryStub();
		DriverFactoryStub.init(factoryMock);
	}

	@Override
	public DriverFactory getFactory(Context context) {
		return factoryMock;
	}

	@Override
	public StorageManager acquireStorageManager() throws OntoDriverException {
		// Do nothing
		return null;
	}

	@Override
	public StorageManager acquireStorageManager(Metamodel metamodel) throws OntoDriverException {
		// Do nothing
		return null;
	}

	@Override
	public StorageManager acquireStorageManager(PersistenceProviderFacade persistenceProvider)
			throws OntoDriverException {
		// Do nothing
		return null;
	}

	public DriverFactoryStub getFactoryMock() {
		return factoryMock;
	}
}

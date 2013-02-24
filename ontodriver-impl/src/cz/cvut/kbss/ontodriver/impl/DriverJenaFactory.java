package cz.cvut.kbss.ontodriver.impl;

import java.util.List;

import cz.cvut.kbss.ontodriver.Context;
import cz.cvut.kbss.ontodriver.DriverAbstractFactory;
import cz.cvut.kbss.ontodriver.OntoDriverException;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.StorageConnector;
import cz.cvut.kbss.ontodriver.StorageModule;

public class DriverJenaFactory extends DriverAbstractFactory {

	public DriverJenaFactory(List<OntologyStorageProperties> storageProperties)
			throws OntoDriverException {
		super(storageProperties);
	}

	@Override
	public StorageModule createStorageModule(Context ctx, boolean autoCommit)
			throws OntoDriverException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public StorageConnector createStorageConnector(Context ctx,
			boolean autoCommit) throws OntoDriverException {
		// TODO Auto-generated method stub
		return null;
	}

}

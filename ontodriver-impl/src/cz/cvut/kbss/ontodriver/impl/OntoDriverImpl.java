package cz.cvut.kbss.ontodriver.impl;

import java.util.List;

import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.ontodriver.OntoDriver;
import cz.cvut.kbss.ontodriver.OntoDriverException;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.StorageManager;

public class OntoDriverImpl implements OntoDriver {

	public OntoDriverImpl(List<OntologyStorageProperties> properties) {
		// TODO Auto-generated constructor stub
	}

	/**
	 * @throws UnsupportedOperationException
	 */
	public StorageManager acquireStorageManager() throws OntoDriverException {
		throw new UnsupportedOperationException();
	}

	public StorageManager acquireStorageManager(Metamodel metamodel) throws OntoDriverException {
		if (metamodel == null) {
			return acquireStorageManager();
		}
		// TODO Auto-generated method stub
		return null;
	}
}

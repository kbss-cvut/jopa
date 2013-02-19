package cz.cvut.kbss.ontodriver.impl;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;

import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.ontodriver.OntoDriver;
import cz.cvut.kbss.ontodriver.OntoDriverException;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.StorageManager;

public class OntoDriverImpl implements OntoDriver {

	private static final Logger LOG = Logger.getLogger(OntoDriverImpl.class.getName());

	private final Map<String, String> properties;

	public OntoDriverImpl(List<OntologyStorageProperties> storageProperties,
			Map<String, String> properties) {
		if (storageProperties == null) {
			throw new NullPointerException("Storage properties cannot be null.");
		}
		if (properties == null) {
			properties = Collections.emptyMap();
		}
		this.properties = properties;
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

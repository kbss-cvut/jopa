package cz.cvut.kbss.ontodriver.sesame.connector;

import java.util.Map;

import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;

public abstract class ConnectorFactory {

	private static ConnectorFactory instance = new ConnectorFactoryImpl();

	protected ConnectorFactory() {
	}

	public static synchronized ConnectorFactory getInstance() {
		// TODO Is this the correct way?
		if (!instance.isOpen()) {
			instance = new ConnectorFactoryImpl();
		}
		return instance;
	}

	public abstract Connector createStorageConnector(OntologyStorageProperties storageProperties,
			Map<String, String> properties) throws SesameDriverException;

	public abstract void close() throws OntoDriverException;
	
	public abstract boolean isOpen();
}

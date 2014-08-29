package cz.cvut.kbss.ontodriver.sesame.connector;

import java.util.Map;

import cz.cvut.kbss.ontodriver.OntologyStorageProperties;

public final class ConnectorFactory {

	private static boolean open = true;

	private ConnectorFactory() {
		throw new AssertionError();
	}

	public static Connector createStorageConnector(OntologyStorageProperties storageProperties,
			Map<String, String> properties) {
		// TODO
		return null;
	}

	public static void close() {
		if (!open) {
			return;
		}
		// TODO
		open = false;
	}

}

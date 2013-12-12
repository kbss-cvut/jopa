package cz.cvut.kbss.jopa.test.utils;

import cz.cvut.kbss.ontodriver.OntologyConnectorType;

public final class StorageInfo {

	private final OntologyConnectorType connectorType;
	private final StorageType storageType;

	public StorageInfo(OntologyConnectorType connectorType, StorageType storageType) {
		if (connectorType == null || storageType == null) {
			throw new NullPointerException();
		}
		this.connectorType = connectorType;
		this.storageType = storageType;
	}

	public OntologyConnectorType getConnectorType() {
		return connectorType;
	}

	public StorageType getStorageType() {
		return storageType;
	}
}

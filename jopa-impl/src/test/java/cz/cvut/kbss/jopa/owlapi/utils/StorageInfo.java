package cz.cvut.kbss.jopa.owlapi.utils;

import cz.cvut.kbss.ontodriver.OntologyConnectorType;
import cz.cvut.kbss.ontodriver.impl.owlapi.OwlapiStorageType;

public final class StorageInfo {

	private final OntologyConnectorType connectorType;
	private final OwlapiStorageType storageType;

	public StorageInfo(OntologyConnectorType connectorType, OwlapiStorageType storageType) {
		if (connectorType == null || storageType == null) {
			throw new NullPointerException();
		}
		this.connectorType = connectorType;
		this.storageType = storageType;
	}

	public OntologyConnectorType getConnectorType() {
		return connectorType;
	}

	public OwlapiStorageType getStorageType() {
		return storageType;
	}
}

package cz.cvut.kbss.ontodriver.impl;

import cz.cvut.kbss.ontodriver.Closeable;
import cz.cvut.kbss.ontodriver.Context;
import cz.cvut.kbss.ontodriver.OntoDriverException;

public abstract class ConnectorFactory implements Closeable {

	private boolean open;

	public ConnectorFactory() {
		// TODO Auto-generated constructor stub
	}

	@Override
	public void close() throws OntoDriverException {
		// TODO Auto-generated method stub

	}

	@Override
	public boolean isOpen() {
		return open;
	}

	/**
	 * Acquires an OWLAPI storage connector. </p>
	 * 
	 * The strategy for connector management is implementation dependent, they
	 * can be created on demand or there can be a pool of connectors.
	 * 
	 * @param autoCommit
	 *            {@code true} if the connector will be used for auto commit
	 *            operation
	 * @param context
	 *            Context containing information about storage the connector
	 *            should be connected to
	 * @return {@code OwlapiStorageConnector}
	 * @throws OntoDriverException
	 *             If called on a closed factory or if an ontology access error
	 *             occurs
	 */
	public abstract OwlapiStorageConnector acquireOwlapiStorageConnector(boolean autoCommit,
			Context context) throws OntoDriverException;

	/**
	 * Acquires a Jena storage connector. </p>
	 * 
	 * The strategy for connector management is implementation dependent, they
	 * can be created on demand or there can be a pool of connectors.
	 * 
	 * @param autoCommit
	 *            {@code true} if the connector will be used for auto commit
	 *            operation
	 * @param context
	 *            Context containing information about storage the connector
	 *            should be connected to
	 * @return {@code JenaStorageConnector}
	 * @throws OntoDriverException
	 *             If called on a closed factory or if an ontology access error
	 *             occurs
	 */
	public abstract OwlapiStorageConnector acquireJenaStorageConnector(boolean autoCommit,
			Context context) throws OntoDriverException;
}

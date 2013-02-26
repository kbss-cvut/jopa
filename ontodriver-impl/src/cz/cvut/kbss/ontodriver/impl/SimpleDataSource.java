package cz.cvut.kbss.ontodriver.impl;

import java.util.Collections;
import java.util.List;
import java.util.Map;

import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.DataSource;
import cz.cvut.kbss.ontodriver.OntoDriver;
import cz.cvut.kbss.ontodriver.OntoDriverProperties;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.StorageManager;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;

/**
 * Simple data source implementation without any pooling. </p>
 * 
 * For each request a new connection is created.
 * 
 * @author kidney
 * 
 */
public class SimpleDataSource implements DataSource {

	private final OntoDriver driver;
	private final Map<String, String> properties;

	public SimpleDataSource(List<OntologyStorageProperties> storageProperties,
			Map<String, String> properties) {
		super();
		if (storageProperties == null) {
			throw new NullPointerException("StorageProperties cannot be null.");
		}
		if (properties == null) {
			properties = Collections.emptyMap();
		}
		this.properties = properties;
		this.driver = new OntoDriverImpl(storageProperties, properties);
	}

	/**
	 * @throws UnsupportedOperationException
	 */
	public Connection getConnection() throws OntoDriverException {
		throw new UnsupportedOperationException();
	}

	public Connection getConnection(Metamodel metamodel) throws OntoDriverException {
		return createConnection(metamodel);
	}

	private Connection createConnection(Metamodel metamodel) throws OntoDriverException {
		final StorageManager sm = driver.acquireStorageManager(metamodel);
		final Connection conn = new ConnectionImpl(sm, metamodel);
		final String strAutoCommit = properties.get(OntoDriverProperties.CONNECTION_AUTO_COMMIT);
		boolean autoCommit = false;
		if (strAutoCommit != null) {
			autoCommit = Boolean.parseBoolean(strAutoCommit);
		}
		conn.setAutoCommit(autoCommit);
		return conn;
	}
}

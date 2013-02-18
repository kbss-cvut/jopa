package cz.cvut.kbss.ontodriver.impl;

import java.util.List;

import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.DataSource;
import cz.cvut.kbss.ontodriver.OntoDriver;
import cz.cvut.kbss.ontodriver.OntoDriverException;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.StorageManager;

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

	public SimpleDataSource(List<OntologyStorageProperties> properties) {
		super();
		if (properties == null) {
			throw new NullPointerException("Properties cannot be null.");
		}
		this.driver = new OntoDriverImpl(properties);
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
		return conn;
	}
}

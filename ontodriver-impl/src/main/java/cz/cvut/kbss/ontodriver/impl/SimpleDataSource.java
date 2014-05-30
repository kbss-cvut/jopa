package cz.cvut.kbss.ontodriver.impl;

import java.util.Collections;
import java.util.Map;
import java.util.Objects;

import cz.cvut.kbss.jopa.utils.ErrorUtils;
import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.DataSource;
import cz.cvut.kbss.ontodriver.OntoDriver;
import cz.cvut.kbss.ontodriver.OntoDriverProperties;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.PersistenceProviderFacade;
import cz.cvut.kbss.ontodriver.StorageModule;
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
	private boolean open;

	public SimpleDataSource(OntologyStorageProperties storageProperties) {
		Objects.requireNonNull(storageProperties,
				ErrorUtils.constructNPXMessage("storageProperties"));
		this.properties = Collections.emptyMap();
		this.driver = new OntoDriverImpl(storageProperties, properties);
		this.open = true;
	}

	public SimpleDataSource(OntologyStorageProperties storageProperties,
			Map<String, String> properties) {
		super();
		Objects.requireNonNull(storageProperties,
				ErrorUtils.constructNPXMessage("storageProperties"));
		if (properties == null) {
			properties = Collections.emptyMap();
		}
		this.properties = properties;
		this.driver = new OntoDriverImpl(storageProperties, properties);
		this.open = true;
	}

	/**
	 * @throws UnsupportedOperationException
	 */
	public Connection getConnection() throws OntoDriverException {
		throw new UnsupportedOperationException();
	}

	public Connection getConnection(PersistenceProviderFacade persistenceProvider)
			throws OntoDriverException {
		Objects.requireNonNull(persistenceProvider,
				ErrorUtils.constructNPXMessage("persistenceProvider"));
		return createConnection(persistenceProvider);
	}

	private Connection createConnection(PersistenceProviderFacade persistenceProvider)
			throws OntoDriverException {
		final StorageModule sm = driver.acquireStorageModule(persistenceProvider);
		final Connection conn = new ConnectionImpl(sm);
		final String strAutoCommit = properties.get(OntoDriverProperties.CONNECTION_AUTO_COMMIT);
		boolean autoCommit = true;
		if (strAutoCommit != null) {
			autoCommit = Boolean.parseBoolean(strAutoCommit);
		}
		conn.setAutoCommit(autoCommit);
		return conn;
	}

	@Override
	public void close() throws OntoDriverException {
		if (!open) {
			return;
		}
		driver.close();
		this.open = false;
	}

	@Override
	public boolean isOpen() {
		return open;
	}

}

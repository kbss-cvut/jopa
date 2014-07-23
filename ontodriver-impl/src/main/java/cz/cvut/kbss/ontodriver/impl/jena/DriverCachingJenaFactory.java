package cz.cvut.kbss.ontodriver.impl.jena;

import java.util.Map;
import java.util.Objects;
import java.util.logging.Level;

import cz.cvut.kbss.jopa.utils.ErrorUtils;
import cz.cvut.kbss.ontodriver.DriverAbstractFactory;
import cz.cvut.kbss.ontodriver.DriverStatement;
import cz.cvut.kbss.ontodriver.JopaStatement;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.PersistenceProviderFacade;
import cz.cvut.kbss.ontodriver.StorageModule;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver.impl.owlapi.OwlapiBasedCachingJenaModule;
import cz.cvut.kbss.ontodriver.impl.owlapi.OwlapiStatement;

/**
 * This is a better optimized factory for Jena storage modules and connectors.
 * </p>
 * 
 * It uses a single central connector which provides cloned data for storage
 * modules' operations.
 * 
 * @author kidney
 * 
 */
public class DriverCachingJenaFactory extends DriverAbstractFactory {

	private volatile OwlapiBasedJenaConnector centralConnector;

	public DriverCachingJenaFactory(OntologyStorageProperties repositoryProperties,
			Map<String, String> properties) throws OntoDriverException {
		super(repositoryProperties, properties);
	}

	@Override
	public synchronized void close() throws OntoDriverException {
		if (!isOpen()) {
			return;
		}
		super.close();
		if (centralConnector != null) {
			centralConnector.close();
		}
	}

	@Override
	public StorageModule createStorageModule(PersistenceProviderFacade persistenceProvider) throws OntoDriverException {
		ensureParametersAndState(persistenceProvider);
		if (LOG.isLoggable(Level.FINER)) {
			LOG.finer("Creating caching Jena storage module.");
		}
		final StorageModule m = new OwlapiBasedCachingJenaModule(persistenceProvider, this);
		registerModule(m);
		return m;
	}

	@Override
	public JenaCachingStorageConnector createStorageConnector()
			throws OntoDriverException {
		ensureOpen();
		if (LOG.isLoggable(Level.FINER)) {
			LOG.finer("Creating caching Jena storage connector.");
		}
		return createConnectorInternal();
	}

	private synchronized JenaCachingStorageConnector createConnectorInternal()
			throws OntoDriverException {
		if (centralConnector == null) {
			synchronized (this) {
				if (centralConnector == null) {
					createCentralConnector();
				}
			}
		}
		final JenaCachingStorageConnector conn = new JenaCachingStorageConnector(centralConnector);
		registerConnector(conn);
		return conn;
	}

	private void createCentralConnector() throws OntoDriverException {
		final JenaStorageType storageType = DriverJenaFactory.resolveStorageType(storageProperties);
		switch (storageType) {
		case FILE:
			this.centralConnector = new JenaFileStorageConnector(storageProperties, properties);
			break;
		case TDB:
			this.centralConnector = new JenaTDBStorageConnector(storageProperties, properties);
			break;
		default:
			throw new IllegalArgumentException("Unsupported storage type " + storageType);
		}
	}

	@Override
	public DriverStatement createStatement(JopaStatement statement) throws OntoDriverException {
		Objects.requireNonNull(statement, ErrorUtils.constructNPXMessage("statement"));
		return new OwlapiStatement(statement);
	}
}

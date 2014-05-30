package cz.cvut.kbss.ontodriver.impl.owlapi;

import java.util.Map;
import java.util.Objects;
import java.util.logging.Level;

import cz.cvut.kbss.jopa.utils.ErrorUtils;
import cz.cvut.kbss.ontodriver.DriverAbstractFactory;
import cz.cvut.kbss.ontodriver.JopaStatement;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.PersistenceProviderFacade;
import cz.cvut.kbss.ontodriver.StorageModule;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import de.fraunhofer.iitb.owldb.OWLDBManager;

/**
 * This factory provides connectors which share the same central ontology and
 * provide to modules only its clones so that there is only one access point to
 * the underlying storage.
 * 
 * @author kidney
 * 
 */
public class DriverCachingOwlapiFactory extends DriverAbstractFactory {

	private volatile OwlapiStorageConnector centralConnector;
	private boolean owldb;

	public DriverCachingOwlapiFactory(OntologyStorageProperties repositoryProperties,
			Map<String, String> properties) throws OntoDriverException {
		super(repositoryProperties, properties);
	}

	@Override
	public StorageModule createStorageModule(PersistenceProviderFacade persistenceProvider,
			boolean autoCommit) throws OntoDriverException {
		ensureParametersAndState(persistenceProvider);
		if (LOG.isLoggable(Level.FINER)) {
			LOG.finer("Creating OWLAPI modularizing storage module.");
		}
		final CachingOwlapiStorageModule m = new CachingOwlapiStorageModule(persistenceProvider,
				this);
		registerModule(m);
		return m;
	}

	@Override
	public CachingOwlapiStorageConnector createStorageConnector(boolean autoCommit)
			throws OntoDriverException {
		ensureOpen();
		if (LOG.isLoggable(Level.FINER)) {
			LOG.finer("Creating OWLAPI modularizing storage connector.");
		}
		return createConnectorInternal(autoCommit);
	}

	/**
	 * Creates modularizing storage connector. </p>
	 * 
	 * This method has to be synchronized since it performs a put-if-absent
	 * operation on the {@code centralConnectors}.
	 * 
	 * @param repository
	 * @param autoCommit
	 * @return
	 * @throws OntoDriverException
	 */
	private synchronized CachingOwlapiStorageConnector createConnectorInternal(boolean autoCommit)
			throws OntoDriverException {
		if (centralConnector == null) {
			synchronized (this) {
				if (centralConnector == null) {
					createCentralConnector();
				}
			}
		}
		final CachingOwlapiStorageConnector conn = new CachingOwlapiStorageConnector(
				centralConnector);
		registerConnector(conn);
		return conn;
	}

	private void createCentralConnector() throws OntoDriverException {
		final OwlapiStorageType type = DriverOwlapiFactory.resolveStorageType(storageProperties);
		switch (type) {
		case OWLDB:
			this.centralConnector = new OwlapiOwldbStorageConnector(storageProperties, properties);
			this.owldb = true;
			break;
		case FILE:
			this.centralConnector = new OwlapiFileStorageConnector(storageProperties, properties);
			break;
		default:
			throw new UnsupportedOperationException("Unknown storage type " + type);
		}
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
		if (owldb) {
			OWLDBManager.getHibernateProvider().close();
		}
	}

	@Override
	public OwlapiStatement createStatement(JopaStatement statement) throws OntoDriverException {
		ensureOpen();
		Objects.requireNonNull(statement, ErrorUtils.constructNPXMessage("statement"));

		return new OwlapiStatement(statement);
	}
}

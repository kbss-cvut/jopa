package cz.cvut.kbss.ontodriver.impl.jena;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;

import cz.cvut.kbss.jopa.model.Repository;
import cz.cvut.kbss.jopa.model.RepositoryID;
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

	private final Map<RepositoryID, OwlapiBasedJenaConnector> centralConnectors;

	public DriverCachingJenaFactory(List<Repository> repositories,
			Map<RepositoryID, OntologyStorageProperties> repositoryProperties,
			Map<String, String> properties) throws OntoDriverException {
		super(repositories, repositoryProperties, properties);
		this.centralConnectors = new HashMap<>(repositories.size());
	}

	@Override
	public void close() throws OntoDriverException {
		if (!isOpen()) {
			return;
		}
		super.close();
		for (OwlapiBasedJenaConnector c : centralConnectors.values()) {
			c.close();
		}
	}

	@Override
	public StorageModule createStorageModule(RepositoryID repository,
			PersistenceProviderFacade persistenceProvider, boolean autoCommit)
			throws OntoDriverException {
		ensureState(repository, persistenceProvider);
		if (LOG.isLoggable(Level.FINER)) {
			LOG.finer("Creating caching Jena storage module.");
		}
		final StorageModule m = new OwlapiBasedCachingJenaModule(getRepository(repository),
				persistenceProvider, this);
		registerModule(m);
		return m;
	}

	@Override
	public JenaCachingStorageConnector createStorageConnector(RepositoryID repository,
			boolean autoCommit) throws OntoDriverException {
		ensureState(repository);
		if (LOG.isLoggable(Level.FINER)) {
			LOG.finer("Creating caching Jena storage connector.");
		}
		return createConnectorInternal(repository);
	}

	private synchronized JenaCachingStorageConnector createConnectorInternal(RepositoryID repository)
			throws OntoDriverException {
		assert repository != null;
		if (!centralConnectors.containsKey(repository)) {
			createCentralConnector(repository);
		}
		final JenaCachingStorageConnector conn = new JenaCachingStorageConnector(
				centralConnectors.get(repository));
		registerConnector(conn);
		return conn;
	}

	private void createCentralConnector(RepositoryID repository) throws OntoDriverException {
		final OntologyStorageProperties props = storageProperties.get(repository);
		final JenaStorageType storageType = DriverJenaFactory.resolveStorageType(props);
		JenaStorageConnector c = null;
		switch (storageType) {
		case FILE:
			c = new JenaFileStorageConnector(props, properties);
			break;
		case TDB:
			c = new JenaTDBStorageConnector(props, properties);
			break;
		default:
			throw new IllegalArgumentException("Unsupported storage type " + storageType);
		}
		centralConnectors.put(repository, c);
	}

	@Override
	public DriverStatement createStatement(JopaStatement statement) throws OntoDriverException {
		if (statement == null) {
			throw new NullPointerException();
		}
		return new OwlapiStatement(statement);
	}
}

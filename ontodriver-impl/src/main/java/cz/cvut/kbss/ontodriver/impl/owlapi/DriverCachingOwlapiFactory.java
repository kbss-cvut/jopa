package cz.cvut.kbss.ontodriver.impl.owlapi;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.logging.Level;

import cz.cvut.kbss.jopa.model.Repository;
import cz.cvut.kbss.jopa.model.RepositoryID;
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

	private final Map<RepositoryID, OwlapiStorageConnector> centralConnectors;
	private boolean owldb;

	public DriverCachingOwlapiFactory(List<Repository> repositories,
			Map<RepositoryID, OntologyStorageProperties> repositoryProperties,
			Map<String, String> properties) throws OntoDriverException {
		super(repositories, repositoryProperties, properties);
		this.centralConnectors = new HashMap<>(repositories.size());
	}

	@Override
	public StorageModule createStorageModule(RepositoryID repository,
			PersistenceProviderFacade persistenceProvider, boolean autoCommit)
			throws OntoDriverException {
		if (LOG.isLoggable(Level.FINER)) {
			LOG.finer("Creating OWLAPI modularizing storage module.");
		}
		final CachingOwlapiStorageModule m = new CachingOwlapiStorageModule(
				getRepository(repository), persistenceProvider, this);
		registerModule(m);
		return m;
	}

	@Override
	public CachingOwlapiStorageConnector createStorageConnector(RepositoryID repository,
			boolean autoCommit) throws OntoDriverException {
		ensureState(repository);
		if (LOG.isLoggable(Level.FINER)) {
			LOG.finer("Creating OWLAPI modularizing storage connector.");
		}
		return createConnectorInternal(repository, autoCommit);
	}

	/**
	 * Creates modularizing storage connector. </p>
	 * 
	 * This method has to be synchronized since it performs a put-if-absent
	 * operation on the {@code centralConnectors}.
	 * 
	 * @param ctx
	 * @param autoCommit
	 * @return
	 * @throws OntoDriverException
	 */
	private synchronized CachingOwlapiStorageConnector createConnectorInternal(
			RepositoryID repository, boolean autoCommit) throws OntoDriverException {
		assert repository != null;
		if (!centralConnectors.containsKey(repository)) {
			createCentralConnector(repository);
		}
		final CachingOwlapiStorageConnector conn = new CachingOwlapiStorageConnector(
				centralConnectors.get(repository));
		registerConnector(conn);
		return conn;
	}

	private void createCentralConnector(RepositoryID repository) throws OntoDriverException {
		final OntologyStorageProperties p = storageProperties.get(repository);
		final OwlapiStorageType type = DriverOwlapiFactory.resolveStorageType(p);
		OwlapiStorageConnector connector = null;
		switch (type) {
		case OWLDB:
			connector = new OwlapiOwldbStorageConnector(p, properties);
			this.owldb = true;
			break;
		case FILE:
			connector = new OwlapiFileStorageConnector(p, properties);
			break;
		default:
			throw new UnsupportedOperationException("Unknown storage type " + type);
		}
		centralConnectors.put(repository, connector);
	}

	@Override
	public void close() throws OntoDriverException {
		if (!isOpen()) {
			return;
		}
		super.close();
		for (OwlapiStorageConnector c : centralConnectors.values()) {
			c.close();
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

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
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import de.fraunhofer.iitb.owldb.OWLDBManager;

/**
 * This factory provides better optimized OWL API ontology module extracting
 * modules and connectors.
 * 
 * @author kidney
 * 
 */
public class DriverModularizingOwlapiFactory extends DriverAbstractFactory {

	private final Map<RepositoryID, OwlapiStorageConnector> centralConnectors;
	private boolean owldb = false;

	public DriverModularizingOwlapiFactory(List<Repository> repositories,
			Map<RepositoryID, OntologyStorageProperties> repositoryProperties,
			Map<String, String> properties) throws OntoDriverException {
		super(repositories, repositoryProperties, properties);
		this.centralConnectors = new HashMap<>(repositories.size());
	}

	@Override
	public ModularizingOwlapiStorageModule createStorageModule(RepositoryID repository,
			PersistenceProviderFacade persistenceProvider, boolean autoCommit)
			throws OntoDriverException {
		ensureState(repository, persistenceProvider);
		if (LOG.isLoggable(Level.FINER)) {
			LOG.finer("Creating OWLAPI modularizing storage module.");
		}
		final ModularizingOwlapiStorageModule m = new ModularizingOwlapiStorageModule(
				getRepository(repository), persistenceProvider, this);
		registerModule(m);
		return m;
	}

	@Override
	public ModularizingStorageConnector createStorageConnector(RepositoryID repository,
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
	 * operation on the {@code centralConnectors}, which is not thread safe.
	 * 
	 * @param ctx
	 * @param autoCommit
	 * @return
	 * @throws OntoDriverException
	 */
	private synchronized ModularizingStorageConnector createConnectorInternal(RepositoryID repo,
			boolean autoCommit) throws OntoDriverException {
		assert repo != null;
		if (!centralConnectors.containsKey(repo)) {
			createCentralConnector(repo);
		}
		final ModularizingStorageConnector conn = new ModularizingStorageConnector(
				centralConnectors.get(repo));
		registerConnector(conn);
		return conn;
	}

	private void createCentralConnector(RepositoryID repo) throws OntoDriverException {
		final OntologyStorageProperties p = storageProperties.get(repo);
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
		centralConnectors.put(repo, connector);
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
		ModularizingStorageConnector.reset();
	}

	@Override
	public OwlapiStatement createStatement(JopaStatement statement) throws OntoDriverException {
		ensureOpen();
		Objects.requireNonNull(statement, ErrorUtils.constructNPXMessage("statement"));

		return new OwlapiStatement(statement);
	}
}

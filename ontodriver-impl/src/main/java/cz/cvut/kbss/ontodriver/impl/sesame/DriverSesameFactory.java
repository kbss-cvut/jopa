package cz.cvut.kbss.ontodriver.impl.sesame;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;

import cz.cvut.kbss.jopa.model.Repository;
import cz.cvut.kbss.ontodriver.DriverAbstractFactory;
import cz.cvut.kbss.ontodriver.DriverStatement;
import cz.cvut.kbss.ontodriver.JopaStatement;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.PersistenceProviderFacade;
import cz.cvut.kbss.ontodriver.StorageModule;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;

public class DriverSesameFactory extends DriverAbstractFactory {

	private final Map<Repository, SesameStorageConnectorImpl> centralConnectors;

	public DriverSesameFactory(List<Repository> repositories,
			Map<Repository, OntologyStorageProperties> repositoryProperties,
			Map<String, String> properties) throws OntoDriverException {
		super(repositories, repositoryProperties, properties);
		this.centralConnectors = new HashMap<>(repositories.size());
	}

	@Override
	public StorageModule createStorageModule(Repository repository,
			PersistenceProviderFacade persistenceProvider, boolean autoCommit)
			throws OntoDriverException {
		ensureState(repository, persistenceProvider);
		if (LOG.isLoggable(Level.FINER)) {
			LOG.finer("Creating Sesame storage module.");
		}
		final StorageModule m = new SesameStorageModule(repository, persistenceProvider, this);
		registerModule(m);
		return m;
	}

	@Override
	public SesameStorageConnector createStorageConnector(Repository repository, boolean autoCommit)
			throws OntoDriverException {
		ensureParametersAndState(repository);
		if (LOG.isLoggable(Level.FINER)) {
			LOG.finer("Creating Sesame storage connector.");
		}
		return createConnectorImpl(repository, autoCommit);
	}

	@Override
	public DriverStatement createStatement(JopaStatement statement) throws OntoDriverException {
		ensureOpen();
		if (statement == null) {
			throw new NullPointerException();
		}
		return new SesameStatement(statement);
	}

	@Override
	public void close() throws OntoDriverException {
		if (!isOpen()) {
			return;
		}
		super.close();
		for (SesameStorageConnector c : centralConnectors.values()) {
			c.close();
		}
	}

	private SesameStorageConnector createConnectorImpl(Repository repository, boolean autoCommit)
			throws OntoDriverException {
		final OntologyStorageProperties props = storageProperties.get(repository);
		assert props != null;
		SesameStorageConnectorImpl central = null;
		synchronized (this) {
			if (!centralConnectors.containsKey(repository)) {
				if (LOG.isLoggable(Level.FINER)) {
					LOG.finer("Creating central connector for repository " + repository);
				}
				central = new SesameStorageConnectorImpl(props, properties);
				centralConnectors.put(repository, central);
			} else {
				central = centralConnectors.get(repository);
			}
		}
		final SesameStorageConnectorProxy c = new SesameStorageConnectorProxy(central);
		registerConnector(c);
		return c;
	}

}

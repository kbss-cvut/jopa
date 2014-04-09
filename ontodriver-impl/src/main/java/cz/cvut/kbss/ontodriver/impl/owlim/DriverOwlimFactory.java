package cz.cvut.kbss.ontodriver.impl.owlim;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;

import cz.cvut.kbss.jopa.model.Repository;
import cz.cvut.kbss.jopa.model.EntityDescriptor;
import cz.cvut.kbss.ontodriver.DriverAbstractFactory;
import cz.cvut.kbss.ontodriver.DriverStatement;
import cz.cvut.kbss.ontodriver.JopaStatement;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.OwlimOntologyStorageProperties;
import cz.cvut.kbss.ontodriver.PersistenceProviderFacade;
import cz.cvut.kbss.ontodriver.StorageConnector;
import cz.cvut.kbss.ontodriver.StorageModule;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver.impl.jena.JenaCachingStorageConnector;
import cz.cvut.kbss.ontodriver.impl.jena.OwlapiBasedJenaConnector;
import cz.cvut.kbss.ontodriver.impl.owlapi.OwlapiBasedCachingJenaModule;
import cz.cvut.kbss.ontodriver.impl.owlapi.OwlapiStatement;

/**
 * Factory for OWLIM storage connectors. </p>
 * 
 * The current version uses OWLIM's Jena connector and supports only local
 * repositories.
 * 
 * @author kidney
 * 
 */
public class DriverOwlimFactory extends DriverAbstractFactory {

	private final Map<EntityDescriptor, OwlapiBasedJenaConnector> centralConnectors;

	public DriverOwlimFactory(List<Repository> repositories,
			Map<EntityDescriptor, OntologyStorageProperties> repositoryProperties,
			Map<String, String> properties) throws OntoDriverException {
		super(repositories, repositoryProperties, properties);
		this.centralConnectors = new HashMap<EntityDescriptor, OwlapiBasedJenaConnector>();
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
	public StorageModule createStorageModule(EntityDescriptor repository,
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
	public StorageConnector createStorageConnector(EntityDescriptor repository, boolean autoCommit)
			throws OntoDriverException {
		ensureState(repository);
		if (LOG.isLoggable(Level.FINER)) {
			LOG.finer("Creating OWLIM storage connector.");
		}
		return createConnectorInternal(repository);
	}

	private synchronized OwlapiBasedJenaConnector createConnectorInternal(EntityDescriptor repository)
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

	private void createCentralConnector(EntityDescriptor repository) throws OntoDriverException {
		final OntologyStorageProperties p = storageProperties.get(repository);
		if (!(p instanceof OwlimOntologyStorageProperties)) {
			throw new OntoDriverException(
					"The storage properties is not suitable for OWLIM based connectors.");
		}
		final OwlimOntologyStorageProperties owlimP = (OwlimOntologyStorageProperties) p;
		if (LOG.isLoggable(Level.FINE)) {
			LOG.fine("Creating central OWLIM storage connector.");
		}
		final JenaBasedOwlimConnector c = new JenaBasedOwlimConnector(owlimP, properties);
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

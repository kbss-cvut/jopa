package cz.cvut.kbss.ontodriver.impl.owlapi;

import java.net.URI;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.logging.Level;

import cz.cvut.kbss.jopa.model.Repository;
import cz.cvut.kbss.jopa.model.RepositoryID;
import cz.cvut.kbss.ontodriver.Context;
import cz.cvut.kbss.ontodriver.DriverAbstractFactory;
import cz.cvut.kbss.ontodriver.JopaStatement;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.PersistenceProviderFacade;
import cz.cvut.kbss.ontodriver.StorageModule;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver.impl.utils.ErrorUtils;
import de.fraunhofer.iitb.owldb.OWLDBManager;

public class DriverOwlapiFactory extends DriverAbstractFactory {

	private static final String JDBC_SCHEME = "jdbc";
	private boolean owldb = false;

	public DriverOwlapiFactory(List<Repository> repositories,
			Map<RepositoryID, OntologyStorageProperties> storageProperties,
			Map<String, String> properties) throws OntoDriverException {
		super(repositories, storageProperties, properties);
	}

	@Override
	public StorageModule createStorageModule(RepositoryID repository,
			PersistenceProviderFacade persistenceProvider, boolean autoCommit)
			throws OntoDriverException {
		ensureState(repository, persistenceProvider);
		if (LOG.isLoggable(Level.FINER)) {
			LOG.finer("Creating OWLAPI storage module.");
		}
		final StorageModule m = new OwlapiStorageModule(repository, persistenceProvider, this);
		registerModule(m);
		return m;
	}

	@Override
	public OwlapiStorageConnector createStorageConnector(RepositoryID repository, boolean autoCommit)
			throws OntoDriverException {
		ensureState(repository);
		if (LOG.isLoggable(Level.FINER)) {
			LOG.finer("Creating OWLAPI storage connector.");
		}
		final OwlapiStorageConnector c = createConnectorInternal(repository);
		registerConnector(c);
		return c;
	}

	private OwlapiStorageConnector createConnectorInternal(RepositoryID repository)
			throws OntoDriverException {
		final OntologyStorageProperties p = storageProperties.get(repository);
		final OwlapiStorageType type = resolveStorageType(p);
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
		return connector;
	}

	/**
	 * Resolves storage type based on the physical URI scheme.
	 * 
	 * @param props
	 *            Storage properties
	 * @return OwlapiStorageType
	 */
	public static OwlapiStorageType resolveStorageType(OntologyStorageProperties props) {
		final URI u = props.getPhysicalURI();
		if (u.getScheme().equals(JDBC_SCHEME)) {
			return OwlapiStorageType.OWLDB;
		} else {
			return OwlapiStorageType.FILE;
		}
	}

	@Override
	public void close() throws OntoDriverException {
		if (!isOpen()) {
			return;
		}
		super.close();
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

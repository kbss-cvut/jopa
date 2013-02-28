package cz.cvut.kbss.ontodriver.impl.owlapi;

import java.net.URI;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;

import cz.cvut.kbss.ontodriver.Context;
import cz.cvut.kbss.ontodriver.DriverAbstractFactory;
import cz.cvut.kbss.ontodriver.OntologyConnectorType;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.PersistenceProviderFacade;
import cz.cvut.kbss.ontodriver.StorageModule;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver.impl.OntoDriverImpl;

public class DriverOwlapiFactory extends DriverAbstractFactory {

	private static final String JDBC_SCHEME = "jdbc";

	static {
		try {
			OntoDriverImpl.registerFactoryClass(OntologyConnectorType.OWLAPI,
					DriverOwlapiFactory.class);
		} catch (OntoDriverException e) {
			LOG.severe("Unable to register " + DriverOwlapiFactory.class
					+ " at the driver. Message: " + e.getMessage());
		}
	}

	public DriverOwlapiFactory(List<OntologyStorageProperties> storageProperties,
			Map<String, String> properties) throws OntoDriverException {
		super(storageProperties, properties);
	}

	@Override
	public StorageModule createStorageModule(Context ctx,
			PersistenceProviderFacade persistenceProvider, boolean autoCommit)
			throws OntoDriverException {
		ensureState(ctx, persistenceProvider);
		if (LOG.isLoggable(Level.FINER)) {
			LOG.finer("Creating OWLAPI storage module.");
		}
		final StorageModule m = new OwlapiStorageModule(ctx, persistenceProvider, this);
		registerModule(m);
		return m;
	}

	@Override
	public OwlapiStorageConnector createStorageConnector(Context ctx, boolean autoCommit)
			throws OntoDriverException {
		ensureState(ctx);
		if (LOG.isLoggable(Level.FINER)) {
			LOG.finer("Creating OWLAPI storage connector.");
		}
		final OwlapiStorageConnector c = createConnectorInternal(ctx);
		registerConnector(c);
		return c;
	}

	private OwlapiStorageConnector createConnectorInternal(Context ctx) throws OntoDriverException {
		final OntologyStorageProperties p = contextsToProperties.get(ctx);
		final OwlapiStorageType type = resolveStorageType(p);
		OwlapiStorageConnector connector = null;
		switch (type) {
		case OWLDB:
			connector = new OwlapiOwldbStorageConnector(p, properties);
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
	private OwlapiStorageType resolveStorageType(OntologyStorageProperties props) {
		final URI u = props.getPhysicalURI();
		if (u.getScheme().equals(JDBC_SCHEME)) {
			return OwlapiStorageType.OWLDB;
		} else {
			return OwlapiStorageType.FILE;
		}
	}
}

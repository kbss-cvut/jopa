package cz.cvut.kbss.ontodriver.impl.owlapi;

import java.net.URI;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;

import cz.cvut.kbss.ontodriver.JopaStatement;
import cz.cvut.kbss.ontodriver.Context;
import cz.cvut.kbss.ontodriver.DriverAbstractFactory;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.PersistenceProviderFacade;
import cz.cvut.kbss.ontodriver.StorageModule;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import de.fraunhofer.iitb.owldb.OWLDBManager;

public class DriverOwlapiFactory extends DriverAbstractFactory {

	private static final String JDBC_SCHEME = "jdbc";
	private boolean owldb = false;

	public DriverOwlapiFactory(List<Context> contexts,
			Map<Context, OntologyStorageProperties> ctxsToProperties, Map<String, String> properties)
			throws OntoDriverException {
		super(contexts, ctxsToProperties, properties);
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
		if (statement == null) {
			throw new NullPointerException();
		}
		return new OwlapiStatement(statement);
	}
}

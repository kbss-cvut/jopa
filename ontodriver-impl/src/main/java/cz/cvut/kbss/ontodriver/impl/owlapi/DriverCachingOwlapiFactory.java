package cz.cvut.kbss.ontodriver.impl.owlapi;

import java.util.HashMap;
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

/**
 * This factory provides connectors which share the same central ontology and
 * provide to modules only its clones so that there is only one access point to
 * the underlying storage.
 * 
 * @author kidney
 * 
 */
public class DriverCachingOwlapiFactory extends DriverAbstractFactory {

	private final Map<Context, OwlapiStorageConnector> centralConnectors;
	private boolean owldb;

	public DriverCachingOwlapiFactory(List<Context> contexts,
			Map<Context, OntologyStorageProperties> ctxsToProperties, Map<String, String> properties)
			throws OntoDriverException {
		super(contexts, ctxsToProperties, properties);
		this.centralConnectors = new HashMap<Context, OwlapiStorageConnector>();
	}

	@Override
	public StorageModule createStorageModule(Context ctx,
			PersistenceProviderFacade persistenceProvider, boolean autoCommit)
			throws OntoDriverException {
		if (LOG.isLoggable(Level.FINER)) {
			LOG.finer("Creating OWLAPI modularizing storage module.");
		}
		final CachingOwlapiStorageModule m = new CachingOwlapiStorageModule(ctx,
				persistenceProvider, this);
		registerModule(m);
		return m;
	}

	@Override
	public CachingOwlapiStorageConnector createStorageConnector(Context ctx, boolean autoCommit)
			throws OntoDriverException {
		ensureState(ctx);
		if (LOG.isLoggable(Level.FINER)) {
			LOG.finer("Creating OWLAPI modularizing storage connector.");
		}
		return createConnectorInternal(ctx, autoCommit);
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
	private synchronized CachingOwlapiStorageConnector createConnectorInternal(Context ctx,
			boolean autoCommit) throws OntoDriverException {
		assert ctx != null;
		if (!centralConnectors.containsKey(ctx)) {
			createCentralConnector(ctx);
		}
		final CachingOwlapiStorageConnector conn = new CachingOwlapiStorageConnector(
				centralConnectors.get(ctx));
		registerConnector(conn);
		return conn;
	}

	private void createCentralConnector(Context ctx) throws OntoDriverException {
		final OntologyStorageProperties p = contextsToProperties.get(ctx);
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
		centralConnectors.put(ctx, connector);
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
		if (statement == null) {
			throw new NullPointerException();
		}
		return new OwlapiStatement(statement);
	}
}

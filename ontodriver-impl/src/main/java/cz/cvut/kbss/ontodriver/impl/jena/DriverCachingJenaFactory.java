package cz.cvut.kbss.ontodriver.impl.jena;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;

import cz.cvut.kbss.ontodriver.AbstractStatement;
import cz.cvut.kbss.ontodriver.Context;
import cz.cvut.kbss.ontodriver.DriverAbstractFactory;
import cz.cvut.kbss.ontodriver.DriverStatement;
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

	private final Map<Context, OwlapiBasedJenaConnector> centralConnectors;

	public DriverCachingJenaFactory(List<Context> contexts,
			Map<Context, OntologyStorageProperties> ctxsToProperties, Map<String, String> properties)
			throws OntoDriverException {
		super(contexts, ctxsToProperties, properties);
		this.centralConnectors = new HashMap<Context, OwlapiBasedJenaConnector>();
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
	public StorageModule createStorageModule(Context ctx,
			PersistenceProviderFacade persistenceProvider, boolean autoCommit)
			throws OntoDriverException {
		ensureState(ctx, persistenceProvider);
		if (LOG.isLoggable(Level.FINER)) {
			LOG.finer("Creating caching Jena storage module.");
		}
		final StorageModule m = new OwlapiBasedCachingJenaModule(ctx, persistenceProvider, this);
		registerModule(m);
		return m;
	}

	@Override
	public JenaCachingStorageConnector createStorageConnector(Context ctx, boolean autoCommit)
			throws OntoDriverException {
		ensureState(ctx);
		if (LOG.isLoggable(Level.FINER)) {
			LOG.finer("Creating caching Jena storage connector.");
		}
		return createConnectorInternal(ctx);
	}

	private synchronized JenaCachingStorageConnector createConnectorInternal(Context ctx)
			throws OntoDriverException {
		assert ctx != null;
		if (!centralConnectors.containsKey(ctx)) {
			createCentralConnector(ctx);
		}
		final JenaCachingStorageConnector conn = new JenaCachingStorageConnector(
				centralConnectors.get(ctx));
		registerConnector(conn);
		return conn;
	}

	private void createCentralConnector(Context ctx) throws OntoDriverException {
		final OntologyStorageProperties props = contextsToProperties.get(ctx);
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
		centralConnectors.put(ctx, c);
	}

	@Override
	public DriverStatement createStatement(AbstractStatement statement) throws OntoDriverException {
		if (statement == null) {
			throw new NullPointerException();
		}
		return new OwlapiStatement(statement);
	}
}

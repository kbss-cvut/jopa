package cz.cvut.kbss.ontodriver.impl.sesame;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;

import cz.cvut.kbss.ontodriver.JopaStatement;
import cz.cvut.kbss.ontodriver.Context;
import cz.cvut.kbss.ontodriver.DriverAbstractFactory;
import cz.cvut.kbss.ontodriver.DriverStatement;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.PersistenceProviderFacade;
import cz.cvut.kbss.ontodriver.StorageModule;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;

public class DriverSesameFactory extends DriverAbstractFactory {

	private final Map<Context, SesameStorageConnectorImpl> centralConnectors;

	public DriverSesameFactory(List<Context> contexts,
			Map<Context, OntologyStorageProperties> ctxsToProperties, Map<String, String> properties)
			throws OntoDriverException {
		super(contexts, ctxsToProperties, properties);
		this.centralConnectors = new HashMap<>(contexts.size());
	}

	@Override
	public StorageModule createStorageModule(Context ctx,
			PersistenceProviderFacade persistenceProvider, boolean autoCommit)
			throws OntoDriverException {
		ensureState(ctx, persistenceProvider);
		if (LOG.isLoggable(Level.FINER)) {
			LOG.finer("Creating Sesame storage module.");
		}
		final StorageModule m = new SesameStorageModule(ctx, persistenceProvider, this);
		registerModule(m);
		return m;
	}

	@Override
	public SesameStorageConnector createStorageConnector(Context ctx, boolean autoCommit)
			throws OntoDriverException {
		ensureState(ctx);
		if (LOG.isLoggable(Level.FINER)) {
			LOG.finer("Creating Sesame storage connector.");
		}
		return createConnectorImpl(ctx, autoCommit);
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

	private SesameStorageConnector createConnectorImpl(Context ctx, boolean autoCommit)
			throws OntoDriverException {
		final OntologyStorageProperties props = reposToProperties.get(ctx);
		assert props != null;
		SesameStorageConnectorImpl central = null;
		synchronized (this) {
			if (!centralConnectors.containsKey(ctx)) {
				if (LOG.isLoggable(Level.FINER)) {
					LOG.finer("Creating central connector for context " + ctx);
				}
				central = new SesameStorageConnectorImpl(props, properties);
				centralConnectors.put(ctx, central);
			} else {
				central = centralConnectors.get(ctx);
			}
		}
		final SesameStorageConnectorProxy c = new SesameStorageConnectorProxy(central);
		registerConnector(c);
		return c;
	}

}

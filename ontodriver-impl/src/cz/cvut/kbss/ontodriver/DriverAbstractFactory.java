package cz.cvut.kbss.ontodriver;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

public abstract class DriverAbstractFactory implements DriverFactory {

	protected static final Logger LOG = Logger
			.getLogger(DriverAbstractFactory.class.getName());

	protected final List<Context> contexts;
	protected final Map<Context, OntologyStorageProperties> contextsToProperties;
	protected final List<OntologyStorageProperties> storageProperties;
	protected final Map<StorageConnector, StorageConnector> openedConnectors;
	protected final Map<StorageModule, StorageModule> openedModules;

	private boolean open;

	protected DriverAbstractFactory(
			List<OntologyStorageProperties> storageProperties)
			throws OntoDriverException {
		if (storageProperties == null || storageProperties.isEmpty()) {
			throw new OntoDriverException(
					"There has to be at least one storage specified.");
		}
		this.storageProperties = storageProperties;
		this.openedConnectors = new HashMap<StorageConnector, StorageConnector>();
		this.openedModules = new HashMap<StorageModule, StorageModule>();
		this.open = true;
		this.contextsToProperties = new HashMap<Context, OntologyStorageProperties>(
				storageProperties.size());
		this.contexts = initContexts();
	}

	@Override
	public boolean isOpen() {
		return open;
	}

	/**
	 * Default implementation which closes all opened storage modules and
	 * connectors.
	 */
	@Override
	public void close() throws OntoDriverException {
		if (!open) {
			return;
		}
		for (StorageModule m : openedModules.values()) {
			m.close();
		}
		for (StorageConnector c : openedConnectors.values()) {
			c.close();
		}
		this.open = false;
	}

	/**
	 * Default implementation which closes the specified module.
	 */
	@Override
	public void releaseStorageModule(StorageModule module)
			throws OntoDriverException {
		ensureOpen();
		if (module == null) {
			throw new NullPointerException();
		}
		if (LOG.isLoggable(Level.FINER)) {
			LOG.finer("Releasing storage module " + module);
		}
		final StorageModule m = openedModules.remove(module);
		if (m == null) {
			throw new OntoDriverException("Module " + module
					+ " not managed in this factory.");
		}
		m.close();
	}

	/**
	 * Default implementation which closes the specified connector.
	 */
	@Override
	public void releaseStorageConnector(StorageConnector connector)
			throws OntoDriverException {
		ensureOpen();
		if (connector == null) {
			throw new NullPointerException();
		}
		if (LOG.isLoggable(Level.FINER)) {
			LOG.finer("Releasing storage connector " + connector);
		}
		final StorageConnector c = openedConnectors.remove(connector);
		if (c == null) {
			throw new OntoDriverException("Connector " + connector
					+ " not managed in this factory.");
		}
		c.close();
	}

	/**
	 * Ensures that this factory is still open.
	 * 
	 * @throws OntoDriverException
	 *             If the factory is closed
	 */
	protected void ensureOpen() throws OntoDriverException {
		if (!open) {
			throw new OntoDriverException(new IllegalStateException(
					"The factory is closed."));
		}
	}

	/**
	 * Registers the {@code module} among the opened modules managed by this
	 * factory.
	 * 
	 * @param module
	 *            The module to register
	 */
	protected void registerModule(StorageModule module) {
		assert module != null;
		openedModules.put(module, module);
	}

	/**
	 * Registers the {@code connector} among the opened connectors managed by
	 * this factory.
	 * 
	 * @param connector
	 *            The connector to register
	 */
	protected void registerConnector(StorageConnector connector) {
		assert connector != null;
		openedConnectors.put(connector, connector);
	}

	private List<Context> initContexts() {
		final List<Context> ctxs = new ArrayList<Context>(
				storageProperties.size());
		for (OntologyStorageProperties p : storageProperties) {
			final Context ctx = new Context(p.getOntologyURI(),
					p.getConnectorType());
			// TODO Set expressiveness and signature for the context
			ctxs.add(ctx);
			contextsToProperties.put(ctx, p);
		}
		return ctxs;
	}
}

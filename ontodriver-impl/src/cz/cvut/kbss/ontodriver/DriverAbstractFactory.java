package cz.cvut.kbss.ontodriver;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;

public abstract class DriverAbstractFactory implements DriverFactory {

	protected static final Logger LOG = Logger.getLogger(DriverAbstractFactory.class.getName());

	protected final List<Context> contexts;
	protected final Map<Context, OntologyStorageProperties> contextsToProperties;

	protected final Map<StorageConnector, StorageConnector> openedConnectors;
	protected final Map<StorageModule, StorageModule> openedModules;
	protected final Map<String, String> properties;

	private boolean open;

	protected DriverAbstractFactory(List<Context> contexts,
			Map<Context, OntologyStorageProperties> ctxsToProperties, Map<String, String> properties)
			throws OntoDriverException {
		if (contexts == null || contexts.isEmpty()) {
			throw new OntoDriverException("There has to be at least one storage context specified.");
		}
		if (ctxsToProperties == null) {
			throw new NullPointerException();
		}
		if (properties == null) {
			properties = Collections.emptyMap();
		}
		this.contexts = contexts;
		this.contextsToProperties = ctxsToProperties;
		this.openedConnectors = new HashMap<StorageConnector, StorageConnector>();
		this.openedModules = new HashMap<StorageModule, StorageModule>();
		this.open = true;
		this.properties = properties;
	}

	@Override
	public List<Context> getContexts() {
		return Collections.unmodifiableList(contexts);
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
	public void releaseStorageModule(StorageModule module) throws OntoDriverException {
		ensureOpen();
		if (module == null) {
			throw new NullPointerException();
		}
		if (LOG.isLoggable(Level.FINER)) {
			LOG.finer("Releasing storage module " + module);
		}
		final StorageModule m = openedModules.remove(module);
		if (m == null) {
			throw new OntoDriverException("Module " + module + " not managed in this factory.");
		}
		m.close();
	}

	/**
	 * Default implementation which closes the specified connector.
	 */
	@Override
	public void releaseStorageConnector(StorageConnector connector) throws OntoDriverException {
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
			throw new OntoDriverException(new IllegalStateException("The factory is closed."));
		}
	}

	/**
	 * Ensures that this factory is in a valid state. </p>
	 * 
	 * This means:
	 * <ul>
	 * <li>That this factory is open</li>
	 * <li>That {@code ctx} is not null</li>
	 * <li>That {@code ctx} exists in this factory</li>
	 * </ul>
	 * 
	 * @param ctx
	 *            The context to check
	 * @throws OntoDriverException
	 *             If the factory is closed or if the context is not valid
	 * @throws NullPointerException
	 *             If {@code ctx} is {@code null}
	 */
	protected void ensureState(Context ctx) throws OntoDriverException {
		ensureOpen();
		if (ctx == null) {
			throw new NullPointerException("Context cannot be null.");
		}
		if (!contextsToProperties.containsKey(ctx)) {
			throw new OntoDriverException("Context " + ctx + " not found.");
		}
	}

	/**
	 * Ensures that this factory is in valid state. </p>
	 * 
	 * @param ctx
	 * @param persistenceProvider
	 * @throws OntoDriverException
	 * @throws NullPointerException
	 *             If {@code metamodel} is null
	 * @see #ensureState(Context)
	 */
	protected void ensureState(Context ctx, PersistenceProviderFacade persistenceProvider)
			throws OntoDriverException {
		ensureState(ctx);
		if (persistenceProvider == null) {
			throw new NullPointerException("PersistenceProvider cannot be null.");
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
}

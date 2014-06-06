package cz.cvut.kbss.ontodriver;

import java.util.Collections;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;
import java.util.logging.Level;
import java.util.logging.Logger;

import cz.cvut.kbss.jopa.utils.ErrorUtils;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;

public abstract class DriverAbstractFactory implements DriverFactory {

	protected static final Logger LOG = Logger.getLogger(DriverAbstractFactory.class.getName());

	private final Map<StorageConnector, StorageConnector> openedConnectors;
	private final Map<StorageModule, StorageModule> openedModules;
	protected final OntologyStorageProperties storageProperties;
	protected final Map<String, String> properties;

	private boolean open;

	protected DriverAbstractFactory(OntologyStorageProperties storageProperties,
			Map<String, String> properties) {
		Objects.requireNonNull(storageProperties,
				ErrorUtils.constructNPXMessage("storageProperties"));
		if (properties == null) {
			properties = Collections.emptyMap();
		}

		this.storageProperties = storageProperties;
		this.openedConnectors = new ConcurrentHashMap<StorageConnector, StorageConnector>();
		this.openedModules = new ConcurrentHashMap<StorageModule, StorageModule>();
		this.open = true;
		this.properties = properties;
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
	 * @throws IllegalStateException
	 *             If the factory is closed
	 */
	protected void ensureOpen() {
		if (!open) {
			new IllegalStateException("The factory is closed.");
		}
	}

	/**
	 * Ensures that this factory is in correct state. </p>
	 * 
	 * I. e. it is open and the specified provider is not {@code null}.
	 * 
	 * @param provider
	 *            PersistenceProviderFacade
	 */
	protected void ensureParametersAndState(PersistenceProviderFacade provider) {
		ensureOpen();
		Objects.requireNonNull(provider, ErrorUtils.constructNPXMessage("provider"));
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

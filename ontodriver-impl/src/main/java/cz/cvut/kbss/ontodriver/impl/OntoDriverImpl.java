package cz.cvut.kbss.ontodriver.impl;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.logging.Level;
import java.util.logging.Logger;

import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.utils.ErrorUtils;
import cz.cvut.kbss.ontodriver.DriverFactory;
import cz.cvut.kbss.ontodriver.OntoDriver;
import cz.cvut.kbss.ontodriver.OntologyConnectorType;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.PersistenceProviderFacade;
import cz.cvut.kbss.ontodriver.StorageModule;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverInitializationException;
import cz.cvut.kbss.ontodriver.impl.jena.DriverJenaFactory;
import cz.cvut.kbss.ontodriver.impl.owlapi.DriverOwlapiFactory;
import cz.cvut.kbss.ontodriver.impl.owlim.DriverOwlimFactory;
import cz.cvut.kbss.ontodriver.impl.sesame.DriverSesameFactory;

public class OntoDriverImpl implements OntoDriver {

	private static final Logger LOG = Logger.getLogger(OntoDriverImpl.class.getName());

	private static final Map<OntologyConnectorType, Class<? extends DriverFactory>> DEFAULT_FACTORY_CLASSES = initDefaultFactories();

	protected final Map<String, String> properties;
	protected final DriverFactory factory;
	private final OntologyStorageProperties storageProperties;
	private final OntologyConnectorType connectorType;
	private boolean open;

	public OntoDriverImpl(OntologyStorageProperties storageProperties) {
		this.storageProperties = Objects.requireNonNull(storageProperties,
				ErrorUtils.constructNPXMessage("storageProperties"));
		this.connectorType = storageProperties.getConnectorType();
		this.properties = Collections.emptyMap();
		this.factory = initFactory();
		this.open = true;
	}

	public OntoDriverImpl(OntologyStorageProperties storageProperties,
			Map<String, String> properties) {
		this.storageProperties = Objects.requireNonNull(storageProperties,
				ErrorUtils.constructNPXMessage("storageProperties"));
		this.connectorType = storageProperties.getConnectorType();
		if (properties == null) {
			properties = Collections.emptyMap();
		}
		this.properties = properties;
		this.factory = initFactory();
		this.open = true;
	}

	@Override
	public StorageModule acquireStorageModule() throws OntoDriverException {
		ensureOpen();
		return acquireStorageModule(new DefaultPersistenceProvider(null));
	}

	@Override
	public StorageModule acquireStorageModule(Metamodel metamodel) throws OntoDriverException {
		ensureOpen();
		Objects.requireNonNull(metamodel, ErrorUtils.constructNPXMessage("metamodel"));

		return acquireStorageModule(new DefaultPersistenceProvider(metamodel));
	}

	@Override
	public StorageModule acquireStorageModule(PersistenceProviderFacade persistenceProvider)
			throws OntoDriverException {
		ensureOpen();
		Objects.requireNonNull(persistenceProvider,
				ErrorUtils.constructNPXMessage("persistenceProvider"));

		if (LOG.isLoggable(Level.FINER)) {
			LOG.finer("Creating storage module.");
		}
		return factory.createStorageModule(persistenceProvider, false);
	}

	@Override
	public void close() throws OntoDriverException {
		if (!open) {
			return;
		}
		if (LOG.isLoggable(Level.CONFIG)) {
			LOG.config("Closing the OntoDriver.");
		}
		factory.close();
		this.open = false;
	}

	@Override
	public boolean isOpen() {
		return open;
	}

	/**
	 * Creates factory instances based on the registered classes. </p>
	 * 
	 * @return map of connector types and factories for them
	 * @see #registerFactoryClass(OntologyConnectorType, Class)
	 */
	private DriverFactory initFactory() {
		final Constructor<? extends DriverFactory> c = resolveFactoryConstructor();
		if (c == null) {
			throw new OntoDriverInitializationException(
					"No suitable factory found for ontology connector type " + connectorType);
		}
		try {
			final DriverFactory f = c.newInstance(storageProperties, properties);
			return f;
		} catch (InstantiationException | IllegalAccessException | IllegalArgumentException
				| InvocationTargetException ex) {
			throw new OntoDriverInitializationException(
					"Unable to initialize factory for connector type " + connectorType, ex);
		}
	}

	private void ensureOpen() throws IllegalStateException {
		if (!open) {
			throw new IllegalStateException("The OntoDriver is already closed.");
		}
	}

	/**
	 * Gets DriverFactory constructor for the connector type used by this
	 * driver. </p>
	 * 
	 * @return Matching factory constructor
	 * @throws OntoDriverInitializationException
	 *             If the specified factory class does not exist or does not
	 *             have the required constructor
	 */
	private Constructor<? extends DriverFactory> resolveFactoryConstructor()
			throws OntoDriverInitializationException {
		final String property = connectorType.getProperty();
		if (properties.containsKey(property)) {
			// Custom factory class specified, use it for instantiation
			final String factoryClassName = properties.get(property);
			return getCustomFactoryConstructor(factoryClassName);
		} else {
			// If the factory class is not specified, use the default one
			final Class<? extends DriverFactory> cls = DEFAULT_FACTORY_CLASSES.get(connectorType);
			try {
				final Constructor<? extends DriverFactory> ctor = cls.getConstructor(
						OntologyStorageProperties.class, Map.class);
				return ctor;
			} catch (NoSuchMethodException | SecurityException e) {
				// Shouldn't happen
				throw new OntoDriverInitializationException(e);
			}
		}
	}

	/**
	 * Gets constructor for a custom DriverFactory specified by its class name.
	 * </p>
	 * 
	 * The factory class is expected to have a constructor taking a
	 * {@code OntologyStorageProperties} instance and a map of properties (
	 * {@code String} to {@code String}) as arguments. If it doesn't, an
	 * exception is thrown.
	 * 
	 * @param factoryClassName
	 *            Name of the factory class
	 * @return Constructor for the factory
	 */
	private Constructor<? extends DriverFactory> getCustomFactoryConstructor(String factoryClassName) {
		if (factoryClassName.isEmpty()) {
			throw new OntoDriverInitializationException(new IllegalArgumentException(
					"Factory class name cannot be empty."));
		}
		try {
			// Find the class and check for its type
			final Class<?> factoryClass = Class.forName(factoryClassName);
			if (!DriverFactory.class.isAssignableFrom(factoryClass)) {
				throw new OntoDriverInitializationException(new IllegalArgumentException(
						"The factory class " + factoryClassName
								+ " does not implement the DriverFactory interface."));
			}
			// This cast is safe thanks to the check above
			final Constructor<? extends DriverFactory> ctor = (Constructor<? extends DriverFactory>) factoryClass
					.getConstructor(OntologyStorageProperties.class, Map.class);
			return ctor;
		} catch (ClassNotFoundException e) {
			throw new OntoDriverInitializationException(
					"Class " + factoryClassName + " not found.", e);
		} catch (NoSuchMethodException e) {
			throw new OntoDriverInitializationException("Class " + factoryClassName
					+ " does not have the required constructor.", e);
		} catch (SecurityException e) {
			throw new OntoDriverInitializationException(e);
		}
	}

	/**
	 * Initializes default factory classes which are used when no custom factory
	 * class is specified.
	 * 
	 * @return {@code Map}
	 */
	private static Map<OntologyConnectorType, Class<? extends DriverFactory>> initDefaultFactories() {
		final Map<OntologyConnectorType, Class<? extends DriverFactory>> m = new HashMap<OntologyConnectorType, Class<? extends DriverFactory>>();
		m.put(OntologyConnectorType.OWLAPI, DriverOwlapiFactory.class);
		m.put(OntologyConnectorType.JENA, DriverJenaFactory.class);
		m.put(OntologyConnectorType.OWLIM, DriverOwlimFactory.class);
		m.put(OntologyConnectorType.SESAME, DriverSesameFactory.class);
		return m;
	}
}

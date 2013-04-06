package cz.cvut.kbss.ontodriver.impl;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.logging.Level;
import java.util.logging.Logger;

import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.ontodriver.Context;
import cz.cvut.kbss.ontodriver.DriverFactory;
import cz.cvut.kbss.ontodriver.OntoDriver;
import cz.cvut.kbss.ontodriver.OntologyConnectorType;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.PersistenceProviderFacade;
import cz.cvut.kbss.ontodriver.StorageManager;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverInitializationException;
import cz.cvut.kbss.ontodriver.impl.jena.DriverJenaFactory;
import cz.cvut.kbss.ontodriver.impl.owlapi.DriverOwlapiFactory;
import cz.cvut.kbss.ontodriver.impl.utils.OntologyProfileChecker;

public class OntoDriverImpl implements OntoDriver {

	private static final Logger LOG = Logger.getLogger(OntoDriverImpl.class.getName());

	private static final Map<OntologyConnectorType, Class<? extends DriverFactory>> DEFAULT_FACTORY_CLASSES = initDefaultFactories();

	private final Map<OntologyConnectorType, Constructor<? extends DriverFactory>> factoryClasses;

	protected final Map<String, String> properties;
	protected final Map<OntologyConnectorType, DriverFactory> factories;
	/** Reference for easier access */
	protected final List<Context> contexts;

	protected OntoDriverImpl() {
		super();
		this.factoryClasses = new HashMap<OntologyConnectorType, Constructor<? extends DriverFactory>>();
		this.properties = Collections.emptyMap();
		this.factories = new HashMap<OntologyConnectorType, DriverFactory>();
		this.contexts = new ArrayList<Context>();
	}

	public OntoDriverImpl(List<OntologyStorageProperties> storageProperties) {
		if (storageProperties == null || storageProperties.isEmpty()) {
			throw new IllegalArgumentException(
					"Storage properties cannot be neither null nor empty.");
		}
		this.factoryClasses = new HashMap<OntologyConnectorType, Constructor<? extends DriverFactory>>();
		this.contexts = new ArrayList<Context>(storageProperties.size());
		this.properties = Collections.emptyMap();
		final Map<Context, OntologyStorageProperties> contextToProps = resolveContexts(storageProperties);
		this.factories = initFactories(contextToProps);
	}

	public OntoDriverImpl(List<OntologyStorageProperties> storageProperties,
			Map<String, String> properties) {
		if (storageProperties == null || storageProperties.isEmpty()) {
			throw new IllegalArgumentException(
					"Storage properties cannot be neither null nor empty.");
		}
		if (properties == null) {
			properties = Collections.emptyMap();
		}
		this.factoryClasses = new HashMap<OntologyConnectorType, Constructor<? extends DriverFactory>>();
		this.properties = properties;
		this.contexts = new ArrayList<Context>(storageProperties.size());
		final Map<Context, OntologyStorageProperties> contextToProps = resolveContexts(storageProperties);
		this.factories = initFactories(contextToProps);
	}

	@Override
	public StorageManager acquireStorageManager() throws OntoDriverException {
		final StorageManager m = new StorageManagerImpl(new DefaultPersistenceProvider(null),
				contexts, this);
		return m;
	}

	@Override
	public StorageManager acquireStorageManager(Metamodel metamodel) throws OntoDriverException {
		if (metamodel == null) {
			throw new NullPointerException("Metamodel cannot be null.");
		}
		final StorageManager m = new StorageManagerImpl(new DefaultPersistenceProvider(metamodel),
				contexts, this);
		return m;
	}

	@Override
	public StorageManager acquireStorageManager(PersistenceProviderFacade persistenceProvider)
			throws OntoDriverException {
		if (persistenceProvider == null) {
			throw new NullPointerException("PersistenceProviderFacade cannot be null.");
		}
		if (LOG.isLoggable(Level.FINER)) {
			LOG.finer("Creating storage manager.");
		}
		final StorageManager m = new StorageManagerImpl(persistenceProvider, contexts, this);
		return m;
	}

	/**
	 * Retrieves {@code DriverFactory} appropriate for the specified
	 * {@code context}. </p>
	 * 
	 * The factory type is determined by the {@code OntologyConnectorType} of
	 * the {@code context}.
	 * 
	 * @param context
	 *            Context
	 * @return Factory for the context or null if there is none registered for
	 *         its connector type
	 * @throws NullPointerException
	 *             If {@code context} is null
	 */
	public DriverFactory getFactory(Context context) {
		if (context == null) {
			throw new NullPointerException("Context cannot be null.");
		}
		return factories.get(context.getConnectorType());
	}

	/**
	 * Creates factory instances based on the registered classes. </p>
	 * 
	 * @param props
	 *            Storage properties containing data about ontology storages
	 * @return map of connector types and factories for them
	 * @see #registerFactoryClass(OntologyConnectorType, Class)
	 */
	private Map<OntologyConnectorType, DriverFactory> initFactories(
			Map<Context, OntologyStorageProperties> ctxsToPros) {
		registerFactoryClasses();
		final Map<OntologyConnectorType, DriverFactory> facts = new HashMap<OntologyConnectorType, DriverFactory>();
		for (Entry<OntologyConnectorType, Constructor<? extends DriverFactory>> e : factoryClasses
				.entrySet()) {
			final Constructor<? extends DriverFactory> c = e.getValue();
			try {
				final DriverFactory f = c.newInstance(contexts, ctxsToPros, properties);
				facts.put(e.getKey(), f);
			} catch (InstantiationException ex) {
				LOG.severe("Unable to initialize factory. + " + e.toString());
			} catch (IllegalAccessException ex) {
				LOG.severe("Unable to initialize factory. + " + e.toString());
			} catch (IllegalArgumentException ex) {
				LOG.severe("Unable to initialize factory. + " + e.toString());
			} catch (InvocationTargetException ex) {
				LOG.severe("Unable to initialize factory. + " + e.toString());
			}
		}
		return facts;
	}

	/**
	 * Resolves contexts from the specified storage properties list. </p>
	 * 
	 * @param storageProperties
	 *            List of storage properties
	 * @return Map with storage properties mapped by the resolved contexts
	 */
	private Map<Context, OntologyStorageProperties> resolveContexts(
			List<OntologyStorageProperties> storageProperties) {
		assert storageProperties != null;
		final Map<Context, OntologyStorageProperties> contextsToProperties = new HashMap<Context, OntologyStorageProperties>();
		for (OntologyStorageProperties p : storageProperties) {
			final Context ctx = new Context(p.getOntologyURI(), p.getConnectorType());
			contexts.add(ctx);
			contextsToProperties.put(ctx, p);
		}
		OntologyProfileChecker.checkProfiles(contextsToProperties);
		return contextsToProperties;
	}

	/**
	 * Registers factory classes based on properties specified to the driver.
	 * </p>
	 * 
	 * Each factory class is expected to have a constructor taking a list of
	 * {@code Context}s, a map of {@code Context} to
	 * {@code OntologyStorageProperties} and a map of properties ({@code String}
	 * to {@code String}) as arguments. If it doesn't an exception is thrown.
	 * 
	 * @param type
	 *            Type of connector to associate the factory class with
	 * @param factoryClass
	 *            The factory class
	 * @throws OntoDriverInitializationException
	 *             If a specified factory class does not exist or does not have
	 *             the required constructor
	 */
	private void registerFactoryClasses() throws OntoDriverInitializationException {
		for (OntologyConnectorType t : OntologyConnectorType.values()) {
			final String property = t.getProperty();
			if (properties.containsKey(property)) {
				// Custom factory class specified, register it for instantiation
				final String factoryClassName = properties.get(property);
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
							.getConstructor(List.class, Map.class, Map.class);
					factoryClasses.put(t, ctor);
				} catch (ClassNotFoundException e) {
					throw new OntoDriverInitializationException("Class " + factoryClassName
							+ " not found.", e);
				} catch (NoSuchMethodException e) {
					throw new OntoDriverInitializationException("Class " + factoryClassName
							+ " does not have the required constructor.", e);
				} catch (SecurityException e) {
					throw new OntoDriverInitializationException(e);
				}
			} else {
				// If the factory class is not specified, use the default one
				final Class<? extends DriverFactory> cls = DEFAULT_FACTORY_CLASSES.get(t);
				try {
					final Constructor<? extends DriverFactory> ctor = cls.getConstructor(
							List.class, Map.class, Map.class);
					factoryClasses.put(t, ctor);
				} catch (NoSuchMethodException e) {
					// Shouldn't happen
					throw new OntoDriverInitializationException(e);
				} catch (SecurityException e) {
					// Shouldn't happen
					throw new OntoDriverInitializationException(e);
				}
			}
		}
	}

	/**
	 * Initializes default factory classes which are used when no special
	 * factory class is specified.
	 * 
	 * @return {@code Map}
	 */
	private static Map<OntologyConnectorType, Class<? extends DriverFactory>> initDefaultFactories() {
		final Map<OntologyConnectorType, Class<? extends DriverFactory>> m = new HashMap<OntologyConnectorType, Class<? extends DriverFactory>>();
		m.put(OntologyConnectorType.OWLAPI, DriverOwlapiFactory.class);
		m.put(OntologyConnectorType.JENA, DriverJenaFactory.class);
		return m;
	}
}

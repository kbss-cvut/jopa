package cz.cvut.kbss.ontodriver.impl;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
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
import cz.cvut.kbss.ontodriver.impl.jena.DriverJenaFactory;
import cz.cvut.kbss.ontodriver.impl.owlapi.DriverOwlapiFactory;

public class OntoDriverImpl implements OntoDriver {

	private static final Logger LOG = Logger.getLogger(OntoDriverImpl.class.getName());

	private static final Map<OntologyConnectorType, Constructor<? extends DriverFactory>> factoryClasses = new HashMap<OntologyConnectorType, Constructor<? extends DriverFactory>>();

	private final Map<String, String> properties;
	private final Map<OntologyConnectorType, DriverFactory> factories;
	/** Reference for easier access */
	private List<Context> contexts;

	public OntoDriverImpl(List<OntologyStorageProperties> storageProperties) {
		if (storageProperties == null || storageProperties.isEmpty()) {
			throw new IllegalArgumentException(
					"Storage properties cannot be neither null nor empty.");
		}
		this.factories = initFactories(storageProperties);
		this.properties = Collections.emptyMap();
		// Get contexts from the first available factory
		this.contexts = factories.values().iterator().next().getContexts();
	}

	public OntoDriverImpl(List<OntologyStorageProperties> storageProperties,
			Map<String, String> properties) {
		if (storageProperties == null) {
			throw new NullPointerException("Storage properties cannot be null.");
		}
		if (properties == null) {
			properties = Collections.emptyMap();
		}
		this.properties = properties;
		this.factories = initFactories(storageProperties);
		// Get contexts from the first available factory
		this.contexts = factories.values().iterator().next().getContexts();
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
			return acquireStorageManager();
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
			List<OntologyStorageProperties> props) {
		final Map<OntologyConnectorType, DriverFactory> facts = new HashMap<OntologyConnectorType, DriverFactory>();
		addDefaultFactories(facts, props);
		for (Entry<OntologyConnectorType, Constructor<? extends DriverFactory>> e : factoryClasses
				.entrySet()) {
			final Constructor<? extends DriverFactory> c = e.getValue();
			try {
				final DriverFactory f = c.newInstance(props, properties);
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
	 * TODO Temporary solution?
	 * 
	 * @param map
	 * @param storageProperties
	 */
	private void addDefaultFactories(Map<OntologyConnectorType, DriverFactory> map,
			List<OntologyStorageProperties> storageProperties) {
		assert map != null;
		try {
			final DriverFactory owlapiFactory = new DriverOwlapiFactory(storageProperties,
					properties);
			map.put(OntologyConnectorType.OWLAPI, owlapiFactory);
			final DriverFactory jenaFactory = new DriverJenaFactory(storageProperties, properties);
			map.put(OntologyConnectorType.JENA, jenaFactory);
		} catch (OntoDriverException e) {
			LOG.severe("Unable to instantiate default driver factories.");
		}
	}

	/**
	 * Registers the specified factory class for the specified connector type.
	 * </p>
	 * 
	 * The factory class is expected to have a constructor taking a list of
	 * {@code OntologyStorageProperties} and a map of properties ({@code String}
	 * to {@code String}) as arguments. If it doesn't an exception is thrown.
	 * 
	 * @param type
	 *            Type of connector to associate the factory class with
	 * @param factoryClass
	 *            The factory class
	 * @throws OntoDriverException
	 *             If the {@code factoryClass} does not have a constructor
	 *             taking a list of {@code OntologyStorageProperties} as
	 *             argument
	 */
	public static void registerFactoryClass(OntologyConnectorType type,
			Class<? extends DriverFactory> factoryClass) throws OntoDriverException {
		if (type == null || factoryClass == null) {
			throw new NullPointerException();
		}
		try {
			if (!DriverFactory.class.isAssignableFrom(factoryClass)) {
				throw new OntoDriverException("The class " + factoryClass.getName()
						+ " is not an implementation of DriverFactory.");
			}
			final Constructor<? extends DriverFactory> c = factoryClass.getConstructor(List.class,
					Map.class);
			factoryClasses.put(type, c);
		} catch (NoSuchMethodException e) {
			throw new OntoDriverException("The class " + factoryClass.getName()
					+ " does not have the required single param constructor.");
		} catch (SecurityException e) {
			throw new OntoDriverException(e);
		}
	}
}

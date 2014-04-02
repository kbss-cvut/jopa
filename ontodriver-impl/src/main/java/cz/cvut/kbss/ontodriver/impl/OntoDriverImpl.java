package cz.cvut.kbss.ontodriver.impl;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.logging.Level;
import java.util.logging.Logger;

import cz.cvut.kbss.jopa.model.Repository;
import cz.cvut.kbss.jopa.model.RepositoryID;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
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
import cz.cvut.kbss.ontodriver.impl.owlim.DriverOwlimFactory;
import cz.cvut.kbss.ontodriver.impl.sesame.DriverSesameFactory;
import cz.cvut.kbss.ontodriver.impl.utils.ErrorUtils;

public class OntoDriverImpl implements OntoDriver {

	private static final Logger LOG = Logger.getLogger(OntoDriverImpl.class.getName());

	private static final Map<OntologyConnectorType, Class<? extends DriverFactory>> DEFAULT_FACTORY_CLASSES = initDefaultFactories();

	private final Map<OntologyConnectorType, Constructor<? extends DriverFactory>> factoryClasses;

	protected final Map<String, String> properties;
	protected final Map<OntologyConnectorType, DriverFactory> factories;
	private final Map<Integer, OntologyConnectorType> factoryTypes;
	private final Map<RepositoryID, OntologyStorageProperties> storageProperties;
	/** Reference for easier access */
	protected final List<Repository> repositories;
	private boolean open;

	public OntoDriverImpl(List<OntologyStorageProperties> storageProperties) {
		Objects.requireNonNull(storageProperties,
				ErrorUtils.constructNPXMessage("storageProperties"));
		if (storageProperties.isEmpty()) {
			throw new IllegalArgumentException("Storage properties cannot be empty.");
		}
		this.factoryClasses = new HashMap<OntologyConnectorType, Constructor<? extends DriverFactory>>();
		this.repositories = new ArrayList<>(storageProperties.size());
		this.properties = Collections.emptyMap();
		this.storageProperties = new HashMap<>(storageProperties.size());
		this.factoryTypes = new HashMap<>(storageProperties.size());
		this.factories = initFactories();
		this.open = true;
	}

	public OntoDriverImpl(List<OntologyStorageProperties> storageProperties,
			Map<String, String> properties) {
		Objects.requireNonNull(storageProperties,
				ErrorUtils.constructNPXMessage("storageProperties"));
		if (storageProperties.isEmpty()) {
			throw new IllegalArgumentException("Storage properties cannot be empty.");
		}
		if (properties == null) {
			properties = Collections.emptyMap();
		}
		this.factoryClasses = new HashMap<OntologyConnectorType, Constructor<? extends DriverFactory>>();
		this.properties = properties;
		this.factoryTypes = new HashMap<>(storageProperties.size());
		this.repositories = new ArrayList<>(storageProperties.size());
		this.storageProperties = new HashMap<>(storageProperties.size());
		resolveContexts(storageProperties);
		this.factories = initFactories();
		this.open = true;
	}

	@Override
	public StorageManager acquireStorageManager() throws OntoDriverException {
		ensureOpen();
		final StorageManager m = new StorageManagerImpl(new DefaultPersistenceProvider(null),
				repositories, this);
		return m;
	}

	@Override
	public StorageManager acquireStorageManager(Metamodel metamodel) throws OntoDriverException {
		ensureOpen();
		Objects.requireNonNull(metamodel, ErrorUtils.constructNPXMessage("metamodel"));

		final StorageManager m = new StorageManagerImpl(new DefaultPersistenceProvider(metamodel),
				repositories, this);
		return m;
	}

	@Override
	public StorageManager acquireStorageManager(PersistenceProviderFacade persistenceProvider)
			throws OntoDriverException {
		ensureOpen();
		Objects.requireNonNull(persistenceProvider,
				ErrorUtils.constructNPXMessage("persistenceProvider"));

		if (LOG.isLoggable(Level.FINER)) {
			LOG.finer("Creating storage manager.");
		}
		final StorageManager m = new StorageManagerImpl(persistenceProvider, repositories, this);
		return m;
	}

	/**
	 * Retrieves {@code DriverFactory} appropriate for the specified repository.
	 * </p>
	 * 
	 * The factory type is determined by the OntologyConnectorType of the
	 * repository.
	 * 
	 * @param repository
	 *            Ontology repository
	 * @return Factory for the repository or null if there is none registered
	 *         for its connector type
	 * @throws NullPointerException
	 *             If {@code repository} is {@code null}
	 */
	public DriverFactory getFactory(Repository repository) {
		ensureOpen();
		Objects.requireNonNull(repository, ErrorUtils.constructNPXMessage("repository"));
		final Integer repoId = repository.getId();
		if (repoId < 0 || repoId >= repositories.size()) {
			throw new IllegalArgumentException("Unknown repository " + repository);
		}
		return factories.get(factoryTypes.get(repoId));
	}

	@Override
	public void close() throws OntoDriverException {
		if (!open) {
			return;
		}
		if (LOG.isLoggable(Level.CONFIG)) {
			LOG.config("Closing the OntoDriver.");
		}
		for (DriverFactory f : factories.values()) {
			if (f != null && f.isOpen()) {
				f.close();
			}
		}
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
	private Map<OntologyConnectorType, DriverFactory> initFactories() {
		registerFactoryClasses();
		final Map<OntologyConnectorType, DriverFactory> facts = new HashMap<OntologyConnectorType, DriverFactory>();
		for (Entry<OntologyConnectorType, Constructor<? extends DriverFactory>> e : factoryClasses
				.entrySet()) {
			final Constructor<? extends DriverFactory> c = e.getValue();
			try {
				final DriverFactory f = c.newInstance(repositories, storageProperties, properties);
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
	 * @param storageProps
	 *            List of storage properties
	 * @return Map with storage properties mapped by the resolved contexts
	 */
	private void resolveContexts(List<OntologyStorageProperties> storageProps) {
		assert storageProps != null;
		for (OntologyStorageProperties p : storageProps) {
			final Repository r = new Repository(p.getPhysicalURI());
			repositories.add(r);
			factoryTypes.put(r.getId(), p.getConnectorType());
			storageProperties.put(r.createRepositoryID(false), p);
		}
		// TODO
		// OntologyProfileChecker.checkProfiles(contextsToProperties);
	}

	private void ensureOpen() throws IllegalStateException {
		if (!open) {
			throw new IllegalStateException("The OntoDriver is already closed.");
		}
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
				// If the factory class is not specified, u, Map.classse the
				// default one
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
		m.put(OntologyConnectorType.OWLIM, DriverOwlimFactory.class);
		m.put(OntologyConnectorType.SESAME, DriverSesameFactory.class);
		return m;
	}
}

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
import cz.cvut.kbss.ontodriver.OntoDriverException;
import cz.cvut.kbss.ontodriver.OntologyConnectorType;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.StorageManager;

public class OntoDriverImpl implements OntoDriver {

	private static final Logger LOG = Logger.getLogger(OntoDriverImpl.class
			.getName());

	private static final Map<OntologyConnectorType, Constructor<? extends DriverFactory>> factoryClasses = new HashMap<OntologyConnectorType, Constructor<? extends DriverFactory>>();

	private final Map<String, String> properties;
	private final Map<OntologyConnectorType, DriverFactory> factories;

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
		// TODO Auto-generated constructor stub
	}

	/**
	 * @throws UnsupportedOperationException
	 */
	@Override
	public StorageManager acquireStorageManager() throws OntoDriverException {
		throw new UnsupportedOperationException();
	}

	@Override
	public StorageManager acquireStorageManager(Metamodel metamodel)
			throws OntoDriverException {
		if (metamodel == null) {
			return acquireStorageManager();
		}
		if (LOG.isLoggable(Level.FINER)) {
			LOG.finer("Creating storage manager.");
		}
		// TODO Auto-generated method stub
		return null;
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

	private Map<OntologyConnectorType, DriverFactory> initFactories(
			List<OntologyStorageProperties> props) {
		final Map<OntologyConnectorType, DriverFactory> facts = new HashMap<OntologyConnectorType, DriverFactory>();
		for (Entry<OntologyConnectorType, Constructor<? extends DriverFactory>> e : factoryClasses
				.entrySet()) {
			final Constructor<? extends DriverFactory> c = e.getValue();
			try {
				final DriverFactory f = c.newInstance(props);
				facts.put(e.getKey(), f);
			} catch (InstantiationException | IllegalAccessException
					| IllegalArgumentException | InvocationTargetException e1) {
				LOG.severe("Unable to initialize factory. + " + e.toString());
			}
		}
		return facts;
	}

	/**
	 * Registers the specified factory class for the specified connector type.
	 * </p>
	 * 
	 * The factory class is expected to have a constructor taking a list of
	 * {@code OntologyStorageProperties} as argument. If it doesn't an exception
	 * is thrown.
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
			Class<? extends DriverFactory> factoryClass)
			throws OntoDriverException {
		if (type == null || factoryClass == null) {
			throw new NullPointerException();
		}
		try {
			if (!DriverFactory.class.isAssignableFrom(factoryClass)) {
				throw new OntoDriverException("The class "
						+ factoryClass.getName()
						+ " is not an implementation of DriverFactory.");
			}
			final Constructor<? extends DriverFactory> c = factoryClass
					.getConstructor(Map.class);
			factoryClasses.put(type, c);
		} catch (NoSuchMethodException e) {
			throw new OntoDriverException("The class " + factoryClass.getName()
					+ " does not have the required single param constructor.");
		} catch (SecurityException e) {
			throw new OntoDriverException(e);
		}
	}
}

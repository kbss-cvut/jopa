package cz.cvut.kbss.ontodriver.test;

import java.lang.reflect.Constructor;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.owlapi.EntityManagerFactoryImpl;
import cz.cvut.kbss.jopa.owlapi.MetamodelImpl;
import cz.cvut.kbss.jopa.owlapi.OWLAPIPersistenceProperties;
import cz.cvut.kbss.jopa.test.TestEnvironment;
import cz.cvut.kbss.jopa.test.utils.EntityManagerFactoryMock;
import cz.cvut.kbss.jopa.test.utils.StorageConfig;
import cz.cvut.kbss.ontodriver.DataSource;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.PersistenceProviderFacade;
import cz.cvut.kbss.ontodriver.impl.SimpleDataSource;

/**
 * Test environment for OntoDriver tests.
 * 
 * @author kidney
 * 
 */
public final class TestEnv {

	private static final String dir = "ontodriverTestResults";

	private static PersistenceProviderFacade providerFacade;

	private TestEnv() {
		throw new AssertionError();
	}

	/**
	 * Creates data source with initialized connection to the ontology storages
	 * defined by {@code storages}. </p>
	 * 
	 * @param baseName
	 *            Base for the ontology IRI
	 * @param storages
	 *            Description of storages
	 * @return {@code DataSource}
	 * @see #createDataSource(String, List, Map)
	 */
	public static DataSource createDataSource(String baseName, List<StorageConfig> storages) {
		return createDataSource(baseName, storages, new HashMap<String, String>());
	}

	/**
	 * Creates data source with initialized connection to the ontology storages
	 * defiend by {@code storages}. </p>
	 * 
	 * This method takes a map of custom properties which are passed to the data
	 * source implementation on creation.
	 * 
	 * @param baseName
	 *            , true Base for the ontology IRI
	 * @param storages
	 *            Description of storages
	 * @param properties
	 *            Custom properties
	 * @return {@code DataSource}
	 */
	public static DataSource createDataSource(String baseName, List<StorageConfig> storages,
			Map<String, String> properties) {
		int i = 1;
		final List<OntologyStorageProperties> storageProperties = new ArrayList<>(storages.size());
		for (StorageConfig c : storages) {
			c.setName(baseName);
			c.setDirectory(dir);
			storageProperties.add(c.createStorageProperties(i++));
		}
		properties.put(OWLAPIPersistenceProperties.REASONER_FACTORY_CLASS,
				TestEnvironment.REASONER_FACTORY_CLASS);
		final DataSource dataSource = new SimpleDataSource(storageProperties, properties);
		return dataSource;
	}

	/**
	 * Retrieves provider facade object which is essential for most of the
	 * operations with the storage.
	 * 
	 * @return {@code PersistenceProviderFacade}
	 */
	public static PersistenceProviderFacade getProviderFacade() {
		if (providerFacade == null) {
			try {
				final EntityManagerFactoryMock emf = new EntityManagerFactoryMock(
						Collections.singletonMap(OWLAPIPersistenceProperties.ENTITY_LOCATION,
								"cz.cvut.kbss.jopa.test"));
				final Constructor<MetamodelImpl> c = MetamodelImpl.class
						.getDeclaredConstructor(EntityManagerFactoryImpl.class);
				c.setAccessible(true);
				final Metamodel metamodel = c.newInstance(emf);
				providerFacade = new PersistenceProviderFacadeMock(metamodel);
			} catch (Exception e) {
				throw new IllegalStateException(e);
			}
		}
		return providerFacade;
	}
}

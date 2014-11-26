package cz.cvut.kbss.ontodriver.test;

import java.lang.reflect.Constructor;
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
	 *            Description of the storage
	 * @return {@code DataSource}
	 * @see #createDataSource(String, List, Map)
	 */
	public static DataSource createDataSource(String baseName, StorageConfig storage) {
		return createDataSource(baseName, storage, new HashMap<String, String>());
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
	 * @param storage
	 *            Description of the storage
	 * @param properties
	 *            Custom properties
	 * @return {@code DataSource}
	 */
	public static DataSource createDataSource(String baseName, StorageConfig storage,
			Map<String, String> properties) {
		storage.setName(baseName);
		storage.setDirectory(dir);
		final OntologyStorageProperties storageProperties = storage.createStorageProperties(0);
		properties.put(OWLAPIPersistenceProperties.REASONER_FACTORY_CLASS,
				TestEnvironment.REASONER_FACTORY_CLASS);
		return new SimpleDataSource(storageProperties, properties);
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
				final EntityManagerFactoryMock emf = new EntityManagerFactoryMock();
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

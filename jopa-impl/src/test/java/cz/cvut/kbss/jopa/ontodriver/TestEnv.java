package cz.cvut.kbss.jopa.ontodriver;

import java.io.File;
import java.lang.reflect.Constructor;
import java.net.URI;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.owlapi.EntityManagerFactoryImpl;
import cz.cvut.kbss.jopa.owlapi.MetamodelImpl;
import cz.cvut.kbss.jopa.owlapi.OWLAPIPersistenceProperties;
import cz.cvut.kbss.jopa.owlapi.utils.EntityManagerFactoryMock;
import cz.cvut.kbss.jopa.owlapi.utils.StorageInfo;
import cz.cvut.kbss.ontodriver.DataSource;
import cz.cvut.kbss.ontodriver.OntologyConnectorType;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.OwldbOntologyStorageProperties;
import cz.cvut.kbss.ontodriver.PersistenceProviderFacade;
import cz.cvut.kbss.ontodriver.impl.SimpleDataSource;
import cz.cvut.kbss.ontodriver.impl.owlapi.OwlapiStorageType;

/**
 * Test environment for OntoDriver tests.
 * 
 * @author kidney
 * 
 */
public final class TestEnv {

	private static final String IRI_BASE = "http://krizik.felk.cvut.cz/ontologies/2013/jopa-tests/";
	private static final String dir = "ontodriverTestResults";
	private static final String DB_URI = "jdbc:postgresql://localhost/owldb";
	private static final String DB_USERNAME = "owldb";
	private static final String DB_PASSWORD = "owldb";
	private static final String DB_DRIVER = "org.postgresql.Driver";
	private static final String REASONER_FACTORY_CLASS = "com.clarkparsia.pellet.owlapiv3.PelletReasonerFactory";

	public static boolean deleteOntologyFile = true;

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
	 * @return DataSource
	 */
	public static DataSource createDataSource(String baseName, List<StorageInfo> storages) {
		int i = 1;
		final Map<String, String> properties = new HashMap<String, String>();
		final List<OntologyStorageProperties> storageProperties = new ArrayList<OntologyStorageProperties>(
				storages.size());
		for (StorageInfo e : storages) {
			final String name = baseName + e.getConnectorType().toString() + (i++);
			final URI ontoUri = URI.create(IRI_BASE + name);
			URI physicalUri = null;
			switch (e.getStorageType()) {
			case FILE:
				final File url = new File(dir + "/" + name + ".owl");
				if (url.exists() && deleteOntologyFile) {
					url.delete();
				}
				physicalUri = url.toURI();
				break;
			case OWLDB:
				physicalUri = URI.create(DB_URI);
				break;
			}
			storageProperties.add(createStorageProperties(ontoUri, physicalUri,
					e.getConnectorType(), e.getStorageType()));
		}
		properties.put(OWLAPIPersistenceProperties.REASONER_FACTORY_CLASS, REASONER_FACTORY_CLASS);
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
						Collections.singletonMap("location", "cz.cvut.kbss.jopa.owlapi"));
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

	private static OntologyStorageProperties createStorageProperties(URI ontologyUri,
			URI physicalUri, OntologyConnectorType connector, OwlapiStorageType storage) {
		OntologyStorageProperties p = null;
		switch (storage) {
		case FILE:
			p = new OntologyStorageProperties(ontologyUri, physicalUri, connector);
			break;
		case OWLDB:
			p = OwldbOntologyStorageProperties.ontologyUri(ontologyUri).physicalUri(physicalUri)
					.connectorType(OntologyConnectorType.OWLAPI).username(DB_USERNAME)
					.password(DB_PASSWORD).jdbcDriverClass(DB_DRIVER).build();
			break;
		}
		return p;
	}
}

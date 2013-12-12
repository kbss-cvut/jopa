/**
 * Copyright (C) 2011 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */

package cz.cvut.kbss.jopa.test;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.lang.reflect.Field;
import java.net.URI;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.LogManager;
import java.util.logging.Logger;

import org.junit.Ignore;
import org.semanticweb.owlapi.model.IRI;

import cz.cvut.kbss.jopa.Persistence;
import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.owlapi.OWLAPIPersistenceProperties;
import cz.cvut.kbss.jopa.owlapi.OWLAPIPersistenceProvider;
import cz.cvut.kbss.jopa.test.utils.StorageInfo;
import cz.cvut.kbss.jopa.test.utils.StorageType;
import cz.cvut.kbss.ontodriver.OntologyConnectorType;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.OwldbOntologyStorageProperties;
import cz.cvut.kbss.ontodriver.test.TestEnv;
import de.fraunhofer.iitb.owldb.OWLDBManager;
import de.fraunhofer.iitb.owldb.util.HibernateProvider;

@Ignore
public class TestEnvironment {
	private static final Logger log = Logger.getLogger(TestEnvironment.class.getName());

	public static final String dir = "testResults";
	public static final String DB_URI = "jdbc:postgresql://localhost/owldb";
	public static final String DB_USERNAME = "owldb";
	public static final String DB_PASSWORD = "owldb";
	public static final String DB_DRIVER = "org.postgresql.Driver";

	private static final String REASONER_FACTORY_CLASS = "com.clarkparsia.pellet.owlapiv3.PelletReasonerFactory";

	public static final String DEFAULT_IRI = "http://krizik.felk.cvut.cz/ontologies/2009/jopa-tests/";

	/**
	 * True if the ontology file should be deleted before access to it is
	 * initialized. This effectively means that the test will create the
	 * ontology from scratch. Default value is true.
	 */
	public static boolean shouldDeleteOntologyFile = true;

	// private static final String REASONER_FACTORY_CLASS =
	// "org.semanticweb.HermiT.Reasoner$ReasonerFactory";

	static {
		try {
			// Load java.util.logging configuration
			LogManager.getLogManager().readConfiguration(
					new FileInputStream(
							"src/test/java/cz/cvut/kbss/jopa/resources/logging.properties"));
		} catch (SecurityException | IOException e) {
			e.printStackTrace();
		}
	}

	public static EntityManager getPersistenceConnector(String name) {
		return getPersistenceConnector(name, StorageType.FILE, true);
	}

	public static EntityManager getPersistenceConnector(String name, boolean cache) {
		return getPersistenceConnector(name, StorageType.FILE, cache);
	}

	public static EntityManager getPersistenceConnector(String name, StorageType storage,
			boolean cache) {
		final List<OntologyStorageProperties> storageProps = Collections
				.singletonList(createOwlapiStorageProperties(name, new StorageInfo(
						OntologyConnectorType.OWLAPI, storage)));
		final Map<String, String> params = initParams(cache);
		return Persistence.createEntityManagerFactory("context-name", storageProps, params)
				.createEntityManager();
	}

	public static EntityManager getPersistenceConnector(String baseName,
			List<StorageInfo> storages, boolean cache) {
		final Map<String, String> params = initParams(cache);
		final List<OntologyStorageProperties> storageProps = new ArrayList<OntologyStorageProperties>(
				storages.size());
		String name;
		int i = 1;
		for (StorageInfo si : storages) {
			name = baseName + si.getConnectorType() + (i++);
			final OntologyStorageProperties p = createOwlapiStorageProperties(name, si);
			assert p != null;
			storageProps.add(p);
		}
		return Persistence.createEntityManagerFactory("context-name", storageProps, params)
				.createEntityManager();
	}

	public static EntityManager getPersistenceConnector(String baseName,
			List<StorageInfo> storages, boolean cache, Map<String, String> props) {
		final Map<String, String> params = initParams(cache);
		// Can override default params
		params.putAll(props);
		final List<OntologyStorageProperties> storageProps = new ArrayList<OntologyStorageProperties>(
				storages.size());
		String name;
		int i = 1;
		for (StorageInfo si : storages) {
			name = baseName + si.getConnectorType() + (i++);
			final OntologyStorageProperties p = createOwlapiStorageProperties(name, si);
			assert p != null;
			storageProps.add(p);
		}
		return Persistence.createEntityManagerFactory("context-name", storageProps, params)
				.createEntityManager();
	}

	private static Map<String, String> initParams(boolean cache) {
		final Map<String, String> params = new HashMap<String, String>();
		if (cache) {
			params.put(OWLAPIPersistenceProperties.CACHE_PROPERTY, "on");
		} else {
			params.put(OWLAPIPersistenceProperties.CACHE_PROPERTY, "off");
		}
		/* Set location of the entities (package) */
		params.put(OWLAPIPersistenceProperties.ENTITY_LOCATION, "cz.cvut.kbss.jopa.owlapi");
		params.put(OWLAPIPersistenceProperties.JPA_PERSISTENCE_PROVIDER,
				OWLAPIPersistenceProvider.class.getName());
		params.put(OWLAPIPersistenceProperties.REASONER_FACTORY_CLASS, REASONER_FACTORY_CLASS);
		return params;
	}

	private static OntologyStorageProperties createOwlapiStorageProperties(String name,
			StorageInfo info) {
		final IRI iri = IRI.create(DEFAULT_IRI + name);
		URI physicalUri = null;
		OntologyStorageProperties p = null;
		switch (info.getStorageType()) {
		case FILE:
			// Ontology stored in a file
			final File url = new File(dir + "/" + name + ".owl");
			if (url.exists() && shouldDeleteOntologyFile) {
				url.delete();
			}
			physicalUri = url.toURI();
		case MEMORY:
			// Intentional fall through
			p = new OntologyStorageProperties(iri.toURI(), physicalUri, info.getConnectorType());
			break;
		case OWLDB:
			// OWLDB ontology access
			physicalUri = URI.create(DB_URI);
			p = OwldbOntologyStorageProperties.ontologyUri(iri.toURI()).physicalUri(physicalUri)
					.connectorType(OntologyConnectorType.OWLAPI).username(DB_USERNAME)
					.password(DB_PASSWORD).jdbcDriverClass(DB_DRIVER).build();
		}
		return p;
	}

	public static Logger getLogger() {
		return log;
	}

	public static void resetOwldbHibernateProvider() throws Exception {
		if (!OWLDBManager.getHibernateProvider().isOpen()) {
			final Field f = OWLDBManager.class.getDeclaredField("hibernateProvider");
			f.setAccessible(true);
			f.set(null, new HibernateProvider());
		}
	}

	/**
	 * Deletes all data in the test OWLDB ontology.
	 * 
	 * @throws Exception
	 */
	public static void clearDatabase() throws Exception {
		java.sql.Connection con = null;
		Statement st1 = null;
		Statement st2 = null;
		ResultSet rs = null;
		con = DriverManager.getConnection(TestEnv.DB_URI, TestEnv.DB_USERNAME, TestEnv.DB_PASSWORD);
		st1 = con.createStatement();
		rs = st1.executeQuery("SELECT table_name FROM information_schema.tables WHERE table_schema = 'public'");
		final String deleteStmt = "TRUNCATE ";
		while (rs.next()) {
			final String table = rs.getString(1);
			st2 = con.createStatement();
			st2.executeUpdate(deleteStmt + table + " CASCADE");
			st2.close();
			st2 = null;
		}
		st1.close();
		con.close();
	}
}

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
import java.sql.DriverManager;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.LogManager;
import java.util.logging.Logger;

import org.junit.Ignore;

import cz.cvut.kbss.jopa.Persistence;
import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.owlapi.OWLAPIPersistenceProperties;
import cz.cvut.kbss.jopa.owlapi.OWLAPIPersistenceProvider;
import cz.cvut.kbss.jopa.test.utils.OwlapiStorageConfig;
import cz.cvut.kbss.jopa.test.utils.StorageConfig;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import de.fraunhofer.iitb.owldb.OWLDBManager;
import de.fraunhofer.iitb.owldb.util.HibernateProvider;

@Ignore
public class TestEnvironment {
	private static final Logger LOG = Logger.getLogger(TestEnvironment.class.getName());

	public static final String dir = "testResults";
	public static final String DB_URI = "jdbc:hsqldb:mem:owldb";
	public static final String DB_USERNAME = "sa";
	public static final String DB_PASSWORD = "";
	public static final String DB_DRIVER = "org.hsqldb.jdbc.JDBCDriver";

	public static final String REASONER_FACTORY_CLASS = "com.clarkparsia.pellet.owlapiv3.PelletReasonerFactory";
	public static final String IRI_BASE = "http://krizik.felk.cvut.cz/ontologies/2013/jopa-tests/";

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
							"src/test/java/cz/cvut/kbss/jopa/test/resources/logging.properties"));
		} catch (SecurityException | IOException e) {
			e.printStackTrace();
		}
	}

	/**
	 * Creates persistence connector, with enabled second level cache, for
	 * OWLAPI accessed ontology stored in a file.
	 * 
	 * @param baseName
	 *            Base name used for ontology URI and physical storage path/URI
	 * @return Persistence context
	 */
	public static EntityManager getPersistenceConnector(String name) {
		return getOwlapiPersistenceConnector(name, true);
	}

	/**
	 * Creates persistence connector for OWLAPI accessed ontology stored in a
	 * file.
	 * 
	 * @param baseName
	 *            Base name used for ontology URI and physical storage path/URI
	 * @param cache
	 *            Whether second level cache should be enabled
	 * @return Persistence context
	 */
	public static EntityManager getPersistenceConnector(String name, boolean cache) {
		return getOwlapiPersistenceConnector(name, cache);
	}

	private static EntityManager getOwlapiPersistenceConnector(String name, boolean cache) {
		return getPersistenceConnector(name,
				Collections.<StorageConfig> singletonList(new OwlapiStorageConfig()), cache).get(0);
	}

	public static EntityManager getPersistenceConnector(String name, StorageConfig storage,
			boolean cache, Map<String, String> properties) {
		return getPersistenceConnector(name, Collections.singletonList(storage), cache, properties)
				.get(0);
	}

	/**
	 * Creates persistence connector for the specified list of storages.
	 * 
	 * @param baseName
	 *            Base name used for ontology URI and physical storage path/URI
	 * @param storages
	 *            List of storage configurations
	 * @param cache
	 *            Whether second level cache should be enabled
	 * @return Persistence context
	 */
	public static List<EntityManager> getPersistenceConnector(String baseName,
			List<StorageConfig> storages, boolean cache) {
		return getPersistenceConnector(baseName, storages, cache,
				Collections.<String, String> emptyMap());
	}

	/**
	 * Creates persistence connector for the specified list of storages.
	 * 
	 * @param baseName
	 *            Base name used for ontology URI and physical storage path/URI
	 * @param storages
	 *            List of storage configurations
	 * @param cache
	 *            Whether second level cache should be enabled
	 * @param props
	 *            Additional properties for the persistence provider
	 * @return Persistence context
	 */
	public static List<EntityManager> getPersistenceConnector(String baseName,
			List<StorageConfig> storages, boolean cache, Map<String, String> props) {
		final Map<String, String> params = initParams(cache);
		// Can override default params
		params.putAll(props);
		int i = 1;
		final List<EntityManager> managers = new ArrayList<>(storages.size());
		for (StorageConfig si : storages) {
			si.setName(baseName);
			si.setDirectory(dir);
			final OntologyStorageProperties p = si.createStorageProperties(i++);
			assert p != null;
			final EntityManager em = Persistence.createEntityManagerFactory("context-name_" + i, p,
					params).createEntityManager();
			managers.add(em);
		}
		return managers;
	}

	private static Map<String, String> initParams(boolean cache) {
		final Map<String, String> params = new HashMap<String, String>();
		if (cache) {
			params.put(OWLAPIPersistenceProperties.CACHE_PROPERTY, "on");
		} else {
			params.put(OWLAPIPersistenceProperties.CACHE_PROPERTY, "off");
		}
		/* Set location of the entities (package) */
		// params.put(OWLAPIPersistenceProperties.ENTITY_LOCATION,
		// "cz.cvut.kbss.jopa.test");
		params.put(OWLAPIPersistenceProperties.JPA_PERSISTENCE_PROVIDER,
				OWLAPIPersistenceProvider.class.getName());
		params.put(OWLAPIPersistenceProperties.REASONER_FACTORY_CLASS, REASONER_FACTORY_CLASS);
		return params;
	}

	public static Logger getLogger() {
		return LOG;
	}

	/**
	 * Resets OWLDB Hibernate provider so that it can be opened again.
	 * 
	 * @throws Exception
	 */
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
		java.sql.Connection conn = null;
		conn = DriverManager.getConnection(DB_URI, DB_USERNAME, DB_PASSWORD);
		Statement stmt = conn.createStatement();
		try {
			stmt.execute("TRUNCATE SCHEMA PUBLIC RESTART IDENTITY AND COMMIT NO CHECK");
			conn.commit();
		} finally {
			stmt.close();
		}
		conn.close();
	}

	/**
	 * Removes (recursively) the specified file/directory. </p>
	 * 
	 * The removal is executed only if the file exists and
	 * {@code deleteOntologyFile} is set to {@code true}.
	 * 
	 * @param file
	 *            The file/directory to remove
	 */
	public static void removeOldTestFiles(File file) {
		if (file.exists() && shouldDeleteOntologyFile) {
			if (file.isDirectory()) {
				for (File c : file.listFiles())
					removeOldTestFiles(c);
				file.delete();
			} else {
				if (!file.delete()) {
					throw new RuntimeException("Unable to delete file " + file);
				}
			}
		}
	}
}

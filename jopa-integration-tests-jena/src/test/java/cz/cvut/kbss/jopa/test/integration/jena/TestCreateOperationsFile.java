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
package cz.cvut.kbss.jopa.test.integration.jena;

import cz.cvut.kbss.jopa.exceptions.OWLEntityExistsException;
import cz.cvut.kbss.jopa.exceptions.RollbackException;
import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.owlapi.OWLAPIPersistenceProperties;
import cz.cvut.kbss.jopa.test.environment.JenaStorageConfig;
import cz.cvut.kbss.jopa.test.environment.StorageConfig;
import cz.cvut.kbss.jopa.test.environment.TestEnvironment;
import cz.cvut.kbss.jopa.test.runner.CreateOperationsRunner;
import cz.cvut.kbss.ontodriver.OntoDriverProperties;
import cz.cvut.kbss.ontodriver.exception.PrimaryKeyNotSetException;
import cz.cvut.kbss.ontodriver.impl.jena.DriverCachingJenaFactory;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import java.net.URI;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

public class TestCreateOperationsFile {

	private static final Logger LOG = Logger.getLogger(TestCreateOperationsFile.class.getName());

	private static final StorageConfig storage = initStorage();
	private static final Map<String, String> properties = initProperties();

	private CreateOperationsRunner runner;

	private EntityManager em;

	@Before
	public void setUp() throws Exception {
		runner = new CreateOperationsRunner(LOG);
	}

	@After
	public void tearDown() throws Exception {
		if (em.isOpen()) {
			if (em.getTransaction().isActive()) {
				em.getTransaction().rollback();
			}
			em.close();
			em.getEntityManagerFactory().close();
		}
	}

	@Test
	public void testPersistWithGenerated() {
		em = TestEnvironment.getPersistenceConnector("JenaFilePersistWithGenerated", storage,
				false, properties);
		runner.persistWithGenerated(em, context());
	}

	@Test(expected = PrimaryKeyNotSetException.class)
	public void testPersistWithoutId() {
		em = TestEnvironment.getPersistenceConnector("JenaFilePersistWithoutId", storage, false,
				properties);
		runner.persistWithoutId(em, context());
	}

	@Test(expected = NullPointerException.class)
	public void testPersistNull() {
		em = TestEnvironment.getPersistenceConnector("JenaFilePersistNull", storage, false,
				properties);
		runner.persistNull(em, context());
	}

	@Test
	public void testPersistRollback() {
		em = TestEnvironment.getPersistenceConnector("JenaFilePersistRollback", storage, false,
				properties);
		runner.persistRollback(em, context());
	}

	@Test(expected = RollbackException.class)
	public void testPersistRollbackOnly() {
		em = TestEnvironment.getPersistenceConnector("JenaFilePersistRollbackOnly", storage, false,
				properties);
		runner.persistRollbackOnly(em, context());
	}

	@Test
	public void testPersistCascade() {
		em = TestEnvironment.getPersistenceConnector("JenaFilePersistWithCascade", storage, false,
				properties);
		runner.persistCascade(em, context());
	}

	@Test(expected = OWLEntityExistsException.class)
	public void testPersistTwiceInOne() {
		em = TestEnvironment.getPersistenceConnector("JenaFilePersistTwice", storage, false,
				properties);
		runner.persistTwice(em, context());
	}

	@Test(expected = RollbackException.class)
	public void testPersistWithoutCascade() {
		em = TestEnvironment.getPersistenceConnector("JenaFilePersistWithoutCascade", storage,
				false, properties);
		runner.persistWithoutCascade(em, context());
	}

	@Test(expected = OWLEntityExistsException.class)
	public void testPersistDetached() {
		em = TestEnvironment.getPersistenceConnector("JenaFilePersistDetached", storage, false,
				properties);
		runner.persistDetachedEntity(em, context());
	}

	@Test
	public void testPersistSimpleList() {
		em = TestEnvironment.getPersistenceConnector("JenaFilePersistSimpleList", storage, false,
				properties);
		runner.persistSimpleList(em, context());
	}

	@Test(expected = RollbackException.class)
	public void testPersistSimpleListNoCascade() {
		em = TestEnvironment.getPersistenceConnector("JenaFilePersistSimpleListNoCascade", storage,
				false, properties);
		runner.persistSimpleListNoCascade(em, context());
	}

	@Test
	public void testPersistReferencedList() {
		em = TestEnvironment.getPersistenceConnector("JenaFilePersistReferencedList", storage,
				false, properties);
		runner.persistReferencedList(em, context());
	}

	@Test(expected = RollbackException.class)
	public void testPersistReferencedListNoCascade() {
		em = TestEnvironment.getPersistenceConnector("JenaFilePersistReferencedListNoCascade",
				storage, false, properties);
		runner.persistReferencedListNoCascade(em, context());
	}

	@Test
	public void testPersistSimpleAndReferencedList() {
		em = TestEnvironment.getPersistenceConnector("JenaFilePersistSimpleAndReferencedList",
				storage, false, properties);
		runner.persistSimpleAndReferencedList(em, context());
	}

	@Test
	public void testPersistProperties() {
		em = TestEnvironment.getPersistenceConnector("JenaFilePersistWithProperties", storage,
				false, properties);
		runner.persistProperties(em, context());
	}

	@Test
	public void testPersistPropertiesEmpty() {
		em = TestEnvironment.getPersistenceConnector("JenaFilePersistWithPropertiesEmpty", storage,
				false, properties);
		runner.persistPropertiesEmpty(em, context());
	}

	@Test(expected = OWLEntityExistsException.class)
	public void persistURITwiceInDifferentClasses() {
		em = TestEnvironment.getPersistenceConnector("JenaFilePersistURITwiceInDifferentClasses",
				storage, false, properties);
		runner.persistURITwiceInDifferentClasses(em, context());
	}

	@Test
	public void testPersistAndUpdateAttributeBeforeCommit() {
		em = TestEnvironment.getPersistenceConnector("JenaFilePersistAndUpdateBeforeCommit", storage, false,
				properties);
		runner.setAttributeValueAfterPersistButBeforeCommit(em, context());
	}

	private URI context() {
		// OWLAPI doesn't use contexts
		return null;
	}

	private static StorageConfig initStorage() {
		return new JenaStorageConfig();
	}

	private static Map<String, String> initProperties() {
		final Map<String, String> map = new HashMap<>();
		map.put(OntoDriverProperties.USE_TRANSACTIONAL_ONTOLOGY, Boolean.TRUE.toString());
		map.put(OntoDriverProperties.JENA_DRIVER_FACTORY, DriverCachingJenaFactory.class.getName());
		map.put(OWLAPIPersistenceProperties.LANG, "en");
		return map;
	}
}

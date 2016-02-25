/**
 * Copyright (C) 2016 Czech Technical University in Prague
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

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.owlapi.OWLAPIPersistenceProperties;
import cz.cvut.kbss.jopa.test.environment.JenaTDBStorageConfig;
import cz.cvut.kbss.jopa.test.environment.StorageConfig;
import cz.cvut.kbss.jopa.test.environment.TestEnvironment;
import cz.cvut.kbss.jopa.test.runner.DeleteOperationsRunner;
import cz.cvut.kbss.ontodriver.OntoDriverProperties;
import cz.cvut.kbss.ontodriver.impl.jena.DriverCachingJenaFactory;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import java.net.URI;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

public class TestDeleteOperationsTDB {

	private static final Logger LOG = Logger.getLogger(TestDeleteOperationsTDB.class.getName());

	private static final StorageConfig storage = initStorage();
	private static final Map<String, String> properties = initProperties();

	private DeleteOperationsRunner runner;

	private EntityManager em;

	@Before
	public void setUpBeforeClass() throws Exception {
		runner = new DeleteOperationsRunner(LOG);
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
	public void testRemoveSimple() {
		em = TestEnvironment.getPersistenceConnector("JenaTDBSimpleRemove", storage, false,
				properties);
		runner.removeSimple(em, context());
	}

	@Test
	public void testRemoveReference() {
		em = TestEnvironment.getPersistenceConnector("JenaTDBRemoveReference", storage, true,
				properties);
		runner.removeReference(em, context());
	}

	@Test
	public void testRemoveCascade() {
		em = TestEnvironment.getPersistenceConnector("JenaTDBRemoveCascade", storage, true,
				properties);
		runner.removeCascade(em, context());
	}

	@Test(expected = IllegalArgumentException.class)
	public void testRemoveDetached() {
		em = TestEnvironment.getPersistenceConnector("JenaTDBRemoveDetached", storage, true,
				properties);
		runner.removeDetached(em, context());
	}

	@Test
	public void testRemoveFromSimpleList() {
		em = TestEnvironment.getPersistenceConnector("JenaTDBRemoveFromSimpleList", storage, true,
				properties);
		runner.removeFromSimpleList(em, context());
	}

	@Test
	public void testRemoveFromReferencedList() {
		em = TestEnvironment.getPersistenceConnector("JenaTDBRemoveFromReferencedList", storage,
				true, properties);
		runner.removeFromReferencedList(em, context());
	}

	@Test
	public void testRemoveListOwner() {
		em = TestEnvironment.getPersistenceConnector("JenaTDBRemoveListOwner", storage, true,
				properties);
		runner.removeListOwner(em, context());
	}

	@Test
	public void testRemoveNotYetCommitted() {
		em = TestEnvironment.getPersistenceConnector("JenaTDBRemoveNotYetCommitted", storage, true,
				properties);
		runner.removeNotYetCommitted(em, context());
	}

	private URI context() {
		return null;
	}

	private static StorageConfig initStorage() {
		return new JenaTDBStorageConfig();
	}

	private static Map<String, String> initProperties() {
		final Map<String, String> map = new HashMap<>();
		map.put(OntoDriverProperties.USE_TRANSACTIONAL_ONTOLOGY, Boolean.TRUE.toString());
		map.put(OntoDriverProperties.JENA_DRIVER_FACTORY, DriverCachingJenaFactory.class.getName());
		map.put(OWLAPIPersistenceProperties.LANG, "en");
		return map;
	}
}

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

import cz.cvut.kbss.jopa.exceptions.OWLInferredAttributeModifiedException;
import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.owlapi.OWLAPIPersistenceProperties;
import cz.cvut.kbss.jopa.test.environment.JenaTDBStorageConfig;
import cz.cvut.kbss.jopa.test.environment.StorageConfig;
import cz.cvut.kbss.jopa.test.environment.TestEnvironment;
import cz.cvut.kbss.jopa.test.runner.UpdateOperationsRunner;
import cz.cvut.kbss.ontodriver.OntoDriverProperties;
import cz.cvut.kbss.ontodriver.impl.jena.DriverCachingJenaFactory;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import java.net.URI;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

public class TestUpdateOperationsTDB {

	private static final Logger LOG = Logger.getLogger(TestUpdateOperationsTDB.class.getName());

	private static final StorageConfig storage = initStorage();
	private static final Map<String, String> properties = initProperties();

	private UpdateOperationsRunner runner;

	private EntityManager em;

	@Before
	public void setUpBeforeClass() throws Exception {
		this.runner = new UpdateOperationsRunner(LOG);
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
	public void testMergeSet() throws Exception {
		em = TestEnvironment.getPersistenceConnector("JenaTDBMergeSet", storage, false, properties);
		runner.mergeSet(em, context());
	}

	@Test
	public void testUpdateDataLeaveLazy() throws Exception {
		em = TestEnvironment.getPersistenceConnector("JenaTDBUpdateDataProperty", storage, false,
				properties);
		runner.updateDataPropertyKeepLazyEmpty(em, context());
	}

	@Test
	public void testUpdateDataPropertySetNull() {
		em = TestEnvironment.getPersistenceConnector("JenaTDBUpdateDataPropertyToNull", storage,
				true, properties);
		runner.updateDataPropertySetNull(em, context());
	}

	@Test
	public void testUpdateReference() {
		em = TestEnvironment.getPersistenceConnector("JenaTDBUpdateReference", storage, true,
				properties);
		runner.updateReference(em, context());
	}

	@Test
	public void testMergeDetachedWithChanges() {
		em = TestEnvironment.getPersistenceConnector("JenaTDBUpdateDetached", storage, true,
				properties);
		runner.mergeDetachedWithChanges(em, context());
	}

	@Test
	public void testMergeDetachedCascade() {
		em = TestEnvironment.getPersistenceConnector("JenaTDBUpdateCascade", storage, true,
				properties);
		runner.mergeDetachedCascade(em, context());
	}

    @Test
    public void testMergeDetachedWithObjectPropertyChange() {
        em = TestEnvironment.getPersistenceConnector("JenaTDBUpdateDetachedWithOPChange", storage, true,
                properties);
        runner.mergeDetachedWithUpdatedObjectProperty(em, context());
    }

	@Test
	public void testRemoveFromSimpleList() {
		em = TestEnvironment.getPersistenceConnector("JenaTDBUpdateRemoveFromSimpleList", storage,
				true, properties);
		runner.removeFromSimpleList(em, context());
	}

	@Test
	public void testAddToSimpleList() {
		em = TestEnvironment.getPersistenceConnector("JenaTDBUpdateAddToSimpleList", storage, true,
				properties);
		runner.addToSimpleList(em, context());
	}

	@Test
	public void testClearSimpleList() {
		em = TestEnvironment.getPersistenceConnector("JenaTDBUpdateClearSimpleList", storage, true,
				properties);
		runner.clearSimpleList(em, context());
	}

	@Test
	public void testReplaceSimpleList() {
		em = TestEnvironment.getPersistenceConnector("JenaTDBUpdateReplaceSimpleList", storage,
				true, properties);
		runner.replaceSimpleList(em, context());
	}

	@Test
	public void testRemoveFromReferencedList() {
		em = TestEnvironment.getPersistenceConnector("JenaTDBUpdateRemoveFromReferencedList",
				storage, true, properties);
		runner.removeFromReferencedList(em, context());
	}

	@Test
	public void testAddToReferencedList() {
		em = TestEnvironment.getPersistenceConnector("JenaTDBUpdateAddToReferencedList", storage,
				true, properties);
		runner.addToReferencedList(em, context());
	}

	@Test
	public void testClearReferencedList() {
		em = TestEnvironment.getPersistenceConnector("JenaTDBUpdateClearReferencedList", storage,
				true, properties);
		runner.clearReferencedList(em, context());
	}

	@Test
	public void testReplaceReferencedList() {
		em = TestEnvironment.getPersistenceConnector("JenaTDBUpdateReplaceReferencedList", storage,
				true, properties);
		runner.replaceReferencedList(em, context());
	}

	@Test
	public void testAddNewToProperties() {
		em = TestEnvironment.getPersistenceConnector("JenaTDBUpdateAddNewToProperties", storage,
				false, properties);
		runner.addNewToProperties(em, context());
	}

	@Test
	public void testAddPropertyValue() {
		em = TestEnvironment.getPersistenceConnector("JenaTDBUpdateAddPropertyValue", storage,
				false, properties);
		runner.addPropertyValue(em, context());
	}
	
	@Test
	public void testAddPropertyValueDetached() {
		em = TestEnvironment.getPersistenceConnector("JenaTDBUpdateAddPropertyValueDetached", storage,
				false, properties);
		runner.addPropertyValueDetached(em, context());
	}

	@Test(expected = OWLInferredAttributeModifiedException.class)
	public void testModifyInferredAttribute() {
		em = TestEnvironment.getPersistenceConnector("JenaTDBModifyInferredAttribute", storage,
				false, properties);
		runner.modifyInferredAttribute(em, context());
	}

	private URI context() {
		// OWLAPI storages don't use contexts
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

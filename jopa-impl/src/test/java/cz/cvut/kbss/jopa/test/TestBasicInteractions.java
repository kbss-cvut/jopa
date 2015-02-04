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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.net.URI;
import java.util.Arrays;
import java.util.HashSet;
import java.util.logging.Logger;

import org.junit.After;
import org.junit.Test;

import cz.cvut.kbss.jopa.exceptions.TransactionRequiredException;
import cz.cvut.kbss.jopa.model.EntityManager;

public class TestBasicInteractions {

	private Logger log = TestEnvironment.getLogger();

	private EntityManager em;

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
	public void testPersistFound() {
		this.em = TestEnvironment.getPersistenceConnector("TestBasicInteractions-testPersistFound");
		OWLClassA t = new OWLClassA();

		final URI pk = URI.create("http://new-class");

		t.setUri(pk);

		em.persist(t);

		final OWLClassA a = em.find(OWLClassA.class, pk);

		assertEquals(pk, a.getUri());
        assertEquals(t.getStringAttribute(), a.getStringAttribute());
	}

	@Test
	public void testRemove() {
		this.em = TestEnvironment.getPersistenceConnector("TestBasicInteractions-testRemove");
		OWLClassA t = new OWLClassA();

		final URI pk = URI.create("http://new-class");

		t.setUri(pk);

		em.persist(t);

		log.info("Persisted " + t);
		assertTrue(em.contains(t));

		em.remove(t);

		log.info("Removed " + t);
		assertFalse(em.contains(t));
	}

	@Test
	public void testSingleOWLClassReference() {
		this.em = TestEnvironment
				.getPersistenceConnector("TestBasicInteractions-testSingleOWLClassReference");
		OWLClassA a = new OWLClassA();
		final URI pkA = URI.create("http://newA");
		a.setUri(pkA);

		em.persist(a);

		OWLClassD d = new OWLClassD();
		final URI pkD = URI.create("http://newD");
		d.setUri(pkD);

		d.setOwlClassA(a);

		em.persist(d);
	}

	@Test
	public void testTypes() {
		this.em = TestEnvironment.getPersistenceConnector("TestBasicInteractions-testTypes");

		OWLClassA a = new OWLClassA();
		final URI pkA = URI.create("http://newA");
		a.setUri(pkA);

		a.setTypes(new HashSet<String>(Arrays.asList("http://classA", "http://classB",
				"http://classC")));

		em.getTransaction().begin();
		em.persist(a);
		em.flush();
		em.getTransaction().commit();
		em.clear();

		OWLClassA ax = em.find(OWLClassA.class, pkA);

		assertEquals(a.getTypes().size(), 3);
		assertEquals(ax.getTypes().size(), 3);
	}

	@Test(expected = TransactionRequiredException.class)
	public void testFlushOutsideTransaction() {
		this.em = TestEnvironment
				.getPersistenceConnector("TestBasicInteractions-flushOutsideTransaction");
		OWLClassA a = new OWLClassA();
		final URI pkA = URI.create("http://newA");
		a.setUri(pkA);

		em.getTransaction().begin();
		em.persist(a);
		em.getTransaction().commit();

		final OWLClassA modA = em.find(OWLClassA.class, pkA);
		modA.setStringAttribute("someString");
		em.flush();
		fail("This line should not have been reached.");
	}

	@Test(expected = IllegalArgumentException.class)
	public void testMergeRemovedEntity() {
		this.em = TestEnvironment
				.getPersistenceConnector("TestBasicInteractions-mergeRemovedEntity");
		OWLClassA a = new OWLClassA();
		final URI pkA = URI.create("http://newA");
		a.setUri(pkA);

		em.getTransaction().begin();
		em.persist(a);
		em.getTransaction().commit();

		final OWLClassA toRemove = em.find(OWLClassA.class, pkA);
		assertNotNull(toRemove);
		em.getTransaction().begin();
		em.remove(toRemove);
		em.merge(toRemove);
		fail("This line should not have been reached.");
	}
}

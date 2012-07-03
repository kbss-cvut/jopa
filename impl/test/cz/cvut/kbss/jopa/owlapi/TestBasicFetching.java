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

package cz.cvut.kbss.jopa.owlapi;

import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import junit.framework.TestCase;
import cz.cvut.kbss.jopa.model.EntityManager;

public class TestBasicFetching extends TestCase {

	public void testFetchSimpleData() {
		EntityManager em = TestEnvironment
				.getPersistenceConnector("TestBasicFetching-testFetchSimpleData");

		OWLClassA a = new OWLClassA();
		URI uri = URI.create("http://new#A");
		a.setUri(uri);

		a.setStringAttribute("new-value");

		em.persist(a);

		assertTrue(em.contains(a));

		em.flush();

		em.clear();

		assertFalse(em.contains(a));

		final OWLClassA aX = em.find(OWLClassA.class, uri);

		assertNotNull(aX);

		assertEquals(aX.getStringAttribute(), "new-value");

		em.close();
	}

	public void testChangeValue() {
		EntityManager em = TestEnvironment
				.getPersistenceConnector("TestBasicFetching-testChangeDataValue");

		OWLClassA a = new OWLClassA();
		URI uri = URI.create("http://new#A");
		a.setUri(uri);

		a.setStringAttribute("old-value");

		em.persist(a);

		assertTrue(em.contains(a));

		em.flush();

		em.clear();

		assertFalse(em.contains(a));

		final OWLClassA aX = em.find(OWLClassA.class, uri);

		assertNotNull(aX);

		aX.setStringAttribute("new-value");

		assertEquals(aX.getStringAttribute(), "new-value");

		em.close();
	}

	public void testFetchReferences() {
		EntityManager pc = TestEnvironment
				.getPersistenceConnector("TestBasicFetching-testFetchSimpleReference");

		OWLClassA a = new OWLClassA();
		URI uriA = URI.create("http://new#A");
		a.setUri(uriA);

		a.setStringAttribute("new-value");

		OWLClassD d = new OWLClassD();
		URI uriD = URI.create("http://new#D");
		d.setUri(uriD);

		d.setOwlClassA(a);

		// more references
		OWLClassA a2 = new OWLClassA();
		URI uriA2 = URI.create("http://new#A2");
		a2.setUri(uriA2);
		OWLClassC c = new OWLClassC();
		URI uriC = URI.create("http://new#C");
		c.setUri(uriC);

		c.setReferencedList(Arrays.asList(a, a2));
		c.setSimpleList(Arrays.asList(a, a2));

		pc.persist(a);
		pc.persist(a2);
		pc.persist(c);
		pc.persist(d);

		assertTrue(pc.contains(a));
		assertTrue(pc.contains(d));

		pc.flush();

		pc.clear();

		assertFalse(pc.contains(a));
		assertFalse(pc.contains(a2));
		assertFalse(pc.contains(c));
		assertFalse(pc.contains(d));

		final OWLClassD dX = pc.find(OWLClassD.class, uriD);

		assertNotNull(dX);

		assertEquals(dX.getOwlClassA().getStringAttribute(), "new-value");

		final OWLClassC cX = pc.find(OWLClassC.class, uriC);

		assertEquals(2, cX.getReferencedList().size());

		assertEquals(2, cX.getSimpleList().size());

		pc.close();
	}

	public void testFetchHugeReferences() {
		EntityManager pc = TestEnvironment
				.getPersistenceConnector("TestBasicFetching-testFetchHugeReferences");

		OWLClassC c = new OWLClassC();
		URI uriC = URI.create("http://new#C");
		c.setUri(uriC);

		List<OWLClassA> list = new ArrayList<OWLClassA>();
		for (int i = 0; i < 100; i++) {
			OWLClassA a = new OWLClassA();
			URI uriA = URI.create("http://new#A-" + i);
			a.setUri(uriA);
			a.setStringAttribute("new-value");
			list.add(a);
			pc.persist(a);
		}

		c.setReferencedList(list);

		pc.persist(c);

		pc.flush();

		pc.clear();

		final OWLClassC cX = pc.find(OWLClassC.class, uriC);

		assertEquals(100, cX.getReferencedList().size());

		pc.close();
	}
}

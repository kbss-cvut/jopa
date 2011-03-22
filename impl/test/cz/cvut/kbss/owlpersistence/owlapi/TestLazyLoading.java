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

package cz.cvut.kbss.owlpersistence.owlapi;

import java.net.URI;
import java.util.Collections;

import junit.framework.TestCase;

import cz.cvut.kbss.owlpersistence.model.EntityManager;

public class TestLazyLoading extends TestCase {

	public void testLazyLoading() {
		EntityManager pc = TestEnvironment
				.getPersistenceConnector("TestLazyLoading-testSimple");

		OWLClassA a = new OWLClassA();
		URI uri = URI.create("http://newA");
		a.setUri(uri);

		a.setStringAttribute("new-value");

		OWLClassC c = new OWLClassC();
		URI uriC = URI.create("http://newC");
		c.setUri(uriC);

		c.setReferencedList(Collections.singletonList(a));

		pc.persist(a);
		pc.persist(c);

		pc.flush();

		pc.clear();

		final OWLClassC aX = pc.find(OWLClassC.class, uriC);

		System.out.println(aX.getReferencedList());

		pc.close();
	}
}

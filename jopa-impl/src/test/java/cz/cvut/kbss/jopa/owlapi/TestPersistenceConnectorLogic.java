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

import static org.junit.Assert.fail;

import java.net.URI;
import java.util.logging.Logger;

import org.junit.Test;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.model.OWLPersistenceException;

public class TestPersistenceConnectorLogic {

	private Logger log = TestEnvironment.getLogger();

	@Test
	public void testPersistWithoutPK() {
		EntityManager pc = TestEnvironment
				.getPersistenceConnector("TestPersistenceConnectorLogic-testPersistWithoutPK");
		pc.clear();
		OWLClassA t = new OWLClassA();

		try {
			pc.persist(t);
			fail();
		} catch (OWLPersistenceException e) {
			log.info("Persisting failed - OK : " + e.getMessage());
			return;
		} finally {
			pc.close();
		}
	}

	public void testTwicePersist() {
		EntityManager pc = TestEnvironment
				.getPersistenceConnector("TestPersistenceConnectorLogic-testTwicePersist");
		OWLClassA t = new OWLClassA();

		final URI pk = URI.create("http://newA");
		t.setUri(pk);

		pc.persist(t);

		try {
			pc.persist(t);
		} catch (OWLPersistenceException e) {
			log.info("Persisting failed - OK: " + e.getMessage() + "\nCause: "
					+ e.getCause().getMessage());
			return;
		} finally {
			pc.close();
		}
	}

	public void testTwicePersistDifferentClasses() {
		EntityManager pc = TestEnvironment
				.getPersistenceConnector("TestPersistenceConnectorLogic-testTwicePersistDifferentClasses");
		final URI pk = URI.create("http://newA");

		OWLClassA a = new OWLClassA();
		a.setUri(pk);
		OWLClassB b = new OWLClassB();
		b.setUri(pk);

		pc.persist(a);

		try {
			pc.persist(b);
		} catch (OWLPersistenceException e) {
			log.info("Persisting failed - OK : " + e.getMessage()
					+ "\n Cause: " + e.getCause().getMessage());
			return;
		} finally {
			pc.close();
		}
	}
}

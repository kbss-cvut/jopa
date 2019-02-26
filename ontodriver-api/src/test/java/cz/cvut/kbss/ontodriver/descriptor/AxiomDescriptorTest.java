/**
 * Copyright (C) 2019 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.descriptor;

import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import org.junit.Before;
import org.junit.Test;

import java.net.URI;

import static org.junit.Assert.*;

public class AxiomDescriptorTest {

	private static final URI CONTEXT = URI
			.create("http://krizik.felk.cvut.cz/ontologies/jopa/contextOne");
	private static final Assertion ASSERTION = Assertion.createPropertyAssertion(
			URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/propertyOne"), false);

	private AxiomDescriptor descriptor;

	@Before
	public void setUp() throws Exception {
		descriptor = new AxiomDescriptor(
				NamedResource.create("http://krizik.felk.cvut.cz/ontologies/jopa/entityA"));
	}

	@Test
	public void testSetSubjectContext() {
		assertNull(descriptor.getSubjectContext());
		descriptor.setSubjectContext(CONTEXT);
		assertNotNull(descriptor.getSubjectContext());
		assertEquals(CONTEXT, descriptor.getSubjectContext());
	}

	@Test
	public void testSetAssertionContext() {
		descriptor.addAssertion(ASSERTION);
		assertNull(descriptor.getAssertionContext(ASSERTION));
		descriptor.setAssertionContext(ASSERTION, CONTEXT);
		assertEquals(CONTEXT, descriptor.getAssertionContext(ASSERTION));
	}

	@Test(expected = IllegalArgumentException.class)
	public void testSetAssertionContextInvalid() {
		descriptor.setAssertionContext(ASSERTION, CONTEXT);
		fail("This line should not have been reached.");
	}
}

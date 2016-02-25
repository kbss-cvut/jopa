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
package cz.cvut.kbss.ontodriver.descriptor;

import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;
import org.junit.Before;
import org.junit.Test;

import java.net.URI;
import java.util.List;

import static org.junit.Assert.*;

public class MutationAxiomDescriptorTest {

	private Assertion dpAssertion;

	private AxiomValueDescriptor descriptor;

	@Before
	public void setUp() throws Exception {
		this.dpAssertion = Assertion.createDataPropertyAssertion(URI
				.create("http://krizik.felk.cvut.cz/ontologies/jopa/attributes#A-stringAttribute"),
				false);
		this.descriptor = new AxiomValueDescriptor(
				NamedResource.create("http://krizik.felk.cvut.cz/ontologies/jopa/entityA"));

	}

	@Test(expected = NullPointerException.class)
	public void testMutationAxiomDescriptorNullArg() {
		this.descriptor = new AxiomValueDescriptor(null);
		fail("This line should not have been reached.");
	}

	@Test
	public void testAddAssertionValueExistingAssertion() {
		final Value<String> val = new Value<>("JustASimpleStringValue");
		descriptor.addAssertion(dpAssertion);
		assertTrue(descriptor.getAssertions().contains(dpAssertion));
		descriptor.addAssertionValue(dpAssertion, val);
		assertFalse(descriptor.getAssertionValues(dpAssertion).isEmpty());
	}

	@Test
	public void testAddAssertionValueForPreviouslyNotExistingAssertion() {
		final Value<String> val = new Value<>("JustASimpleStringValue");
		assertFalse(descriptor.getAssertions().contains(dpAssertion));
		descriptor.addAssertionValue(dpAssertion, val);
		assertFalse(descriptor.getAssertionValues(dpAssertion).isEmpty());
		assertTrue(descriptor.getAssertions().contains(dpAssertion));
	}

	@Test
	public void testAddMultipleAssertionValues() {
		final Value<String> val = new Value<>("JustASimpleStringValue");
		final Value<String> val2 = new Value<>("AnotherStringValue");
		descriptor.addAssertionValue(dpAssertion, val);
		descriptor.addAssertionValue(dpAssertion, val2);
		final List<Value<?>> res = descriptor.getAssertionValues(dpAssertion);
		assertEquals(2, res.size());
		assertTrue(res.contains(val));
		assertTrue(res.contains(val2));
	}

	@Test
	public void testGetAssertionValuesEmpty() {
		assertNotNull(descriptor.getAssertionValues(dpAssertion));
		assertTrue(descriptor.getAssertionValues(dpAssertion).isEmpty());
		descriptor.addAssertion(dpAssertion);
		assertTrue(descriptor.getAssertionValues(dpAssertion).isEmpty());
	}

	@Test
	public void testGetAssertionValues() {
		final Value<String> val = new Value<>("JustASimpleStringValue");
		descriptor.addAssertionValue(dpAssertion, val);
		final List<Value<?>> res = descriptor.getAssertionValues(dpAssertion);
		assertEquals(1, res.size());
		assertEquals(val, res.get(0));
	}

	@Test
	public void testAddAssertion() {
		descriptor.addAssertion(dpAssertion);
		assertTrue(descriptor.getAssertions().contains(dpAssertion));
	}

	@Test(expected = NullPointerException.class)
	public void testAddAssertionNull() {
		descriptor.addAssertion(null);
		fail("This line should not have been reached.");
	}
}

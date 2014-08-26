package cz.cvut.kbss.ontodriver_new;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.net.URI;
import java.util.List;

import org.junit.Before;
import org.junit.Test;

import cz.cvut.kbss.ontodriver_new.model.Assertion;
import cz.cvut.kbss.ontodriver_new.model.NamedResource;
import cz.cvut.kbss.ontodriver_new.model.Value;

public class MutationAxiomDescriptorTest {

	private Assertion dpAssertion;

	private MutationAxiomDescriptor descriptor;

	@Before
	public void setUp() throws Exception {
		this.dpAssertion = Assertion.createDataPropertyAssertion(URI
				.create("http://krizik.felk.cvut.cz/ontologies/jopa/attributes#A-stringAttribute"),
				false);
		this.descriptor = new MutationAxiomDescriptor(
				NamedResource.create("http://krizik.felk.cvut.cz/ontologies/jopa/entityA"));

	}

	@Test(expected = NullPointerException.class)
	public void testMutationAxiomDescriptorNullArg() {
		this.descriptor = new MutationAxiomDescriptor(null);
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

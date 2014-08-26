package cz.cvut.kbss.ontodriver_new;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.fail;

import java.net.URI;

import org.junit.Before;
import org.junit.Test;

import cz.cvut.kbss.ontodriver_new.model.Assertion;
import cz.cvut.kbss.ontodriver_new.model.NamedResource;

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

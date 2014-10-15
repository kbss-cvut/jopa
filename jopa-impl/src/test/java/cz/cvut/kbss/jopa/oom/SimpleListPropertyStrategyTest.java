package cz.cvut.kbss.jopa.oom;

import static org.junit.Assert.fail;

import java.net.URI;

import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;

import cz.cvut.kbss.jopa.model.metamodel.EntityType;

public class SimpleListPropertyStrategyTest {
	
	private static final URI PK = URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/entityC");
	
	@Mock
	private EntityType etC;

	@Before
	public void setUp() throws Exception {
	}

	@Test
	public void testAddValueFromAxiom() {
		fail("Not yet implemented");
	}

	@Test
	public void testBuildInstanceFieldValue() {
		fail("Not yet implemented");
	}

}

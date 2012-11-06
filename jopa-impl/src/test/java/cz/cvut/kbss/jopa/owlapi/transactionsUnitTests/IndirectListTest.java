package cz.cvut.kbss.jopa.owlapi.transactionsUnitTests;

import static org.junit.Assert.fail;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

import org.junit.BeforeClass;
import org.junit.Test;

import cz.cvut.kbss.jopa.adapters.IndirectList;
import cz.cvut.kbss.jopa.owlapi.OWLClassA;
import cz.cvut.kbss.jopa.owlapi.OWLClassC;
import cz.cvut.kbss.jopa.owlapi.utils.UnitOfWorkImplStub;
import cz.cvut.kbss.jopa.sessions.UnitOfWorkImpl;

public class IndirectListTest {

	private static final Logger LOG = Logger.getLogger(IndirectListTest.class.getName());

	private static UnitOfWorkImpl uow;
	private static List<OWLClassA> list;
	private static OWLClassC owner;

	private static IndirectList<OWLClassA> target;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		uow = new UnitOfWorkImplStub(null);
		owner = new OWLClassC();
		owner.setUri(URI.create("http://C"));
		list = new ArrayList<OWLClassA>();
		for (byte i = 0; i < 10; i++) {
			OWLClassA a = new OWLClassA();
			a.setUri(URI.create("http://a" + i));
			a.setStringAttribute("testString");
			list.add(a);
		}
		target = new IndirectList<OWLClassA>(owner, uow, list);
		owner.setReferencedList(target);
	}

	@Test
	public void testAddE() {
		fail("Not yet implemented");
	}

	@Test
	public void testAddIntE() {
		fail("Not yet implemented");
	}

	@Test
	public void testAddAllCollectionOfQextendsE() {
		fail("Not yet implemented");
	}

	@Test
	public void testClear() {
		fail("Not yet implemented");
	}

	@Test
	public void testRemoveObject() {
		fail("Not yet implemented");
	}

	@Test
	public void testRemoveInt() {
		fail("Not yet implemented");
	}

	@Test
	public void testRemoveAll() {
		fail("Not yet implemented");
	}

	@Test
	public void testRetainAll() {
		fail("Not yet implemented");
	}

	@Test
	public void testSet() {
		fail("Not yet implemented");
	}

}

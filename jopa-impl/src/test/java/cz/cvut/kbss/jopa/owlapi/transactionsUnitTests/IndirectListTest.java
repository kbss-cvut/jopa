package cz.cvut.kbss.jopa.owlapi.transactionsUnitTests;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import cz.cvut.kbss.jopa.adapters.IndirectList;
import cz.cvut.kbss.jopa.owlapi.OWLClassA;
import cz.cvut.kbss.jopa.owlapi.OWLClassC;
import cz.cvut.kbss.jopa.owlapi.utils.AccessorStub;
import cz.cvut.kbss.jopa.owlapi.utils.ServerSessionStub;
import cz.cvut.kbss.jopa.owlapi.utils.UnitOfWorkImplStub;

public class IndirectListTest {

	private static final Logger LOG = Logger.getLogger(IndirectListTest.class.getName());

	private static UnitOfWorkImplStub uow;
	private static List<OWLClassA> list;
	private static List<OWLClassA> backupList;
	private static OWLClassC owner;

	private static IndirectList<OWLClassA> target;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		LOG.setLevel(Level.ALL);
		uow = new UnitOfWorkImplStub(new ServerSessionStub(new AccessorStub()));
		owner = new OWLClassC();
		owner.setUri(URI.create("http://C"));
		backupList = new ArrayList<OWLClassA>();
		list = new ArrayList<OWLClassA>();
		for (byte i = 0; i < 10; i++) {
			OWLClassA a = new OWLClassA();
			a.setUri(URI.create("http://a" + i));
			a.setStringAttribute("testString");
			backupList.add(a);
		}
		list.addAll(backupList);
		target = new IndirectList<OWLClassA>(owner, uow, list);
		owner.setReferencedList(target);
	}

	@Before
	public void setUp() throws Exception {
		uow.setLastEntity(null);
		list.clear();
		list.addAll(backupList);
	}

	@Test(expected = NullPointerException.class)
	public void testConstructorNull() {
		final IndirectList<OWLClassA> l = new IndirectList<OWLClassA>(owner, uow, null);
		assertNull(l);
	}

	@Test(expected = NullPointerException.class)
	public void testConstructorNullTwo() {
		final IndirectList<OWLClassA> l = new IndirectList<OWLClassA>(owner, null, list);
		assertNull(l);
	}

	@Test
	public void testAddE() {
		LOG.config("Test: add element to the list.");
		final OWLClassA added = new OWLClassA();
		added.setUri(URI.create("http://added"));
		owner.getReferencedList().add(added);
		assertNotNull(uow.getLastEntity());
		assertEquals(owner, uow.getLastEntity());
		assertEquals(backupList.size() + 1, target.size());
	}

	@Test
	public void testAddENull() {
		LOG.config("Test: add null to the list.");
		// Adding null is possible for some list types (ArrayList in our case
		// permits them)
		owner.getReferencedList().add(null);
		assertNotNull(uow.getLastEntity());
		assertEquals(owner, uow.getLastEntity());
		assertEquals(backupList.size() + 1, target.size());
	}

	@Test
	public void testAddIntE() {
		LOG.config("Test: add element to the specified position in the list.");
		final OWLClassA added = new OWLClassA();
		added.setUri(URI.create("http://added"));
		owner.getReferencedList().add(list.size() / 2, added);
		assertNotNull(uow.getLastEntity());
		assertEquals(owner, uow.getLastEntity());
		assertEquals(backupList.size() + 1, target.size());
	}

	@Test
	public void testAddAll() {
		LOG.config("Test: add all elements from another collections.");
		final List<OWLClassA> toAdd = new ArrayList<OWLClassA>();
		for (int i = 0; i < 5; i++) {
			final OWLClassA a = new OWLClassA();
			a.setUri(URI.create("http://addedA" + i));
			toAdd.add(a);
		}
		owner.getReferencedList().addAll(toAdd);
		assertNotNull(uow.getLastEntity());
		assertEquals(owner, uow.getLastEntity());
		assertEquals(backupList.size() + toAdd.size(), target.size());
	}

	@Test
	public void testClear() {
		LOG.config("Test: clear the list.");
		owner.getReferencedList().clear();
		assertNotNull(uow.getLastEntity());
		assertEquals(owner, uow.getLastEntity());
		assertTrue(target.isEmpty());
	}

	@Test
	public void testRemoveObject() {
		LOG.config("Test: remove element from the list.");
		owner.getReferencedList().remove(list.get(0));
		assertNotNull(uow.getLastEntity());
		assertEquals(owner, uow.getLastEntity());
		assertEquals(backupList.size() - 1, target.size());
	}

	@Test
	public void testRemoveNull() {
		LOG.config("Test: remove null from the list.");
		owner.getReferencedList().remove(null);
		assertNull(uow.getLastEntity());
	}

	@Test
	public void testRemoveInt() {
		LOG.config("Test: remove element at index.");
		owner.getReferencedList().remove(list.size() - 1);
		assertNotNull(uow.getLastEntity());
		assertEquals(owner, uow.getLastEntity());
		assertEquals(backupList.size() - 1, target.size());
	}

	@Test
	public void testRemoveAll() {
		LOG.config("Test: remove all elements contained in another collection.");
		List<OWLClassA> toRemove = list.subList(2, 5);
		final int toRemoveSize = toRemove.size();
		owner.getReferencedList().removeAll(toRemove);
		assertNotNull(uow.getLastEntity());
		assertEquals(owner, uow.getLastEntity());
		assertEquals(backupList.size() - toRemoveSize, target.size());
	}

	@Test
	public void testRetainAll() {
		LOG.config("Test: retain all.");
		List<OWLClassA> toRetain = list.subList(2, 5);
		final int toRetainSize = toRetain.size();
		owner.getReferencedList().retainAll(toRetain);
		assertNotNull(uow.getLastEntity());
		assertEquals(owner, uow.getLastEntity());
		assertEquals(toRetainSize, target.size());
	}

	@Test
	public void testSet() {
		LOG.config("Test: set element at the specified position.");
		final OWLClassA a = new OWLClassA();
		a.setUri(URI.create("http://setA"));
		owner.getReferencedList().set(0, a);
		assertNotNull(uow.getLastEntity());
		assertEquals(owner, uow.getLastEntity());
		assertEquals(backupList.size(), target.size());
		assertTrue(target.contains(a));
	}
}

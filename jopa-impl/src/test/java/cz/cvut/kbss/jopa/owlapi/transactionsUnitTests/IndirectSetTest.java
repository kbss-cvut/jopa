package cz.cvut.kbss.jopa.owlapi.transactionsUnitTests;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.net.URI;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import cz.cvut.kbss.jopa.adapters.IndirectSet;
import cz.cvut.kbss.jopa.owlapi.OWLClassA;
import cz.cvut.kbss.jopa.owlapi.OWLClassF;
import cz.cvut.kbss.jopa.owlapi.utils.UnitOfWorkImplStub;

public class IndirectSetTest {

	private static final Logger LOG = Logger.getLogger(IndirectListTest.class.getName());

	private static UnitOfWorkImplStub uow;
	private static Set<OWLClassA> set;
	private static Set<OWLClassA> backupSet;
	private static OWLClassF owner;

	private static IndirectSet<OWLClassA> target;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		LOG.setLevel(Level.ALL);
		uow = new UnitOfWorkImplStub(null);
		owner = new OWLClassF();
		owner.setUri(URI.create("http://C"));
		backupSet = new HashSet<OWLClassA>();
		set = new HashSet<OWLClassA>();
		for (byte i = 0; i < 10; i++) {
			OWLClassA a = new OWLClassA();
			a.setUri(URI.create("http://a" + i));
			a.setStringAttribute("testString");
			backupSet.add(a);
		}
		set.addAll(backupSet);
		target = new IndirectSet<OWLClassA>(owner, uow, set);
		owner.setSimpleSet(target);
	}

	@Before
	public void setUp() throws Exception {
		uow.setLastEntity(null);
		set.clear();
		set.addAll(backupSet);
	}

	@Test(expected = NullPointerException.class)
	public void testIndirectSet() {
		LOG.config("Test: create new IndirectSet with null owner. Should throw exception.");
		final IndirectSet<OWLClassA> res = new IndirectSet<OWLClassA>(owner, null, set);
		assertNull(res);
	}

	@Test
	public void testContains() {
		LOG.config("Test: contains element.");
		final OWLClassA elem = backupSet.iterator().next();
		assertTrue(target.contains(elem));
	}

	@Test
	public void testIteratorHasNext() {
		LOG.config("Test: create iterator and call hasNext.");
		final Iterator<OWLClassA> it = set.iterator();
		final Iterator<OWLClassA> indIt = target.iterator();
		while (it.hasNext()) {
			it.next();
			assertTrue(indIt.hasNext());
			assertNotNull(indIt.next());
		}
	}

	@Test
	public void testIteratorNext() {
		LOG.config("Test: create iterator and call next.");
		final Iterator<OWLClassA> it = set.iterator();
		final Iterator<OWLClassA> indIt = target.iterator();
		while (it.hasNext()) {
			assertTrue(indIt.hasNext());
			assertEquals(it.next(), indIt.next());
		}
	}

	@Test
	public void testIteratorRemove() {
		LOG.config("Test: create iterator and remove and element.");
		final Iterator<OWLClassA> it = target.iterator();
		assertTrue(it.hasNext());
		it.next();
		it.remove();
		assertEquals(owner, uow.getLastEntity());
		assertEquals(backupSet.size() - 1, set.size());
	}

	@Test
	public void testAdd() {
		LOG.config("Test: add an element.");
		final OWLClassA a = new OWLClassA();
		a.setUri(URI.create("http://newA"));
		a.setStringAttribute("testAttribute");
		target.add(a);
		assertEquals(owner, uow.getLastEntity());
		assertEquals(backupSet.size() + 1, set.size());
	}

	@Test
	public void testRemove() {
		LOG.config("Test: remove element from the set.");
		final OWLClassA toRemove = set.iterator().next();
		target.remove(toRemove);
		assertEquals(owner, uow.getLastEntity());
		assertEquals(backupSet.size() - 1, set.size());
	}

	@Test
	public void testAddAll() {
		LOG.config("test: add all elements from another collection.");
		final List<OWLClassA> toAdd = new ArrayList<OWLClassA>();
		for (int i = 0; i < 5; i++) {
			final OWLClassA a = new OWLClassA();
			a.setUri(URI.create("http://addAllA" + i));
			toAdd.add(a);
		}
		target.addAll(toAdd);
		assertEquals(owner, uow.getLastEntity());
		assertEquals(backupSet.size() + toAdd.size(), set.size());
		assertTrue(set.contains(toAdd.iterator().next()));
	}

	@Test
	public void testRetainAll() {
		LOG.config("Test: retain all elements from a collection.");
		Set<OWLClassA> toRetain = new HashSet<OWLClassA>();
		Iterator<OWLClassA> it = backupSet.iterator();
		for (int i = 0; i < 8; i++) {
			assertTrue(it.hasNext());
			toRetain.add(it.next());
		}
		target.retainAll(toRetain);
		assertEquals(owner, uow.getLastEntity());
		assertEquals(toRetain.size(), set.size());
		assertEquals(toRetain.size(), target.size());
	}

	@Test
	public void testRemoveAll() {
		LOG.config("Test: remove all elements from a collection.");
		Set<OWLClassA> toRemove = new HashSet<OWLClassA>();
		Iterator<OWLClassA> it = backupSet.iterator();
		for (int i = 0; i < 8; i++) {
			assertTrue(it.hasNext());
			toRemove.add(it.next());
		}
		target.removeAll(toRemove);
		assertEquals(owner, uow.getLastEntity());
		assertEquals(backupSet.size() - toRemove.size(), set.size());
	}

	@Test(expected = NullPointerException.class)
	public void testRemoveAllNull() {
		LOG.config("Test: remove all. Null.");
		target.removeAll(null);
		fail("This line should not have been reached.");
	}

	@Test
	public void testClear() {
		LOG.config("Test: clear the set.");
		target.clear();
		assertEquals(owner, uow.getLastEntity());
		assertTrue(set.isEmpty());
		assertTrue(target.isEmpty());
	}

}

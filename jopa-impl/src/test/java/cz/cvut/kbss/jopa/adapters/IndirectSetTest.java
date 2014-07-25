package cz.cvut.kbss.jopa.adapters;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import cz.cvut.kbss.jopa.adapters.IndirectSet;
import cz.cvut.kbss.jopa.sessions.UnitOfWorkImpl;
import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.OWLClassF;

public class IndirectSetTest {

	private static Set<OWLClassA> set;
	private static Set<OWLClassA> backupSet;
	private static OWLClassF owner;
	private static Field ownerField;

	private IndirectSet<OWLClassA> target;

	@Mock
	private UnitOfWorkImpl uow;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		owner = new OWLClassF();
		owner.setUri(URI.create("http://C"));
		ownerField = OWLClassF.class.getDeclaredField("simpleSet");
		backupSet = new HashSet<OWLClassA>();
		set = new HashSet<OWLClassA>();
		for (byte i = 0; i < 10; i++) {
			OWLClassA a = new OWLClassA();
			a.setUri(URI.create("http://a" + i));
			a.setStringAttribute("testString");
			backupSet.add(a);
		}
		set.addAll(backupSet);
	}

	@Before
	public void setUp() throws Exception {
		MockitoAnnotations.initMocks(this);
		when(uow.isInTransaction()).thenReturn(Boolean.TRUE);
		target = new IndirectSet<OWLClassA>(owner, ownerField, uow, set);
		set.clear();
		set.addAll(backupSet);
		owner.setSimpleSet(target);
	}

	@Test(expected = NullPointerException.class)
	public void testIndirectSetNullUoW() {
		@SuppressWarnings("unused")
		final IndirectSet<OWLClassA> res = new IndirectSet<OWLClassA>(owner, ownerField, null, set);
		fail("This line should not have been reached.");
	}

	@Test(expected = NullPointerException.class)
	public void testIndirectSetNullReferencedSet() {
		@SuppressWarnings("unused")
		final IndirectSet<OWLClassA> res = new IndirectSet<OWLClassA>(owner, ownerField, uow, null);
		fail("This line should not have been reached.");
	}

	@Test
	public void testContains() {
		final OWLClassA elem = backupSet.iterator().next();
		assertTrue(target.contains(elem));
	}

	@Test
	public void testIteratorHasNext() {
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
		final Iterator<OWLClassA> it = set.iterator();
		final Iterator<OWLClassA> indIt = target.iterator();
		while (it.hasNext()) {
			assertTrue(indIt.hasNext());
			assertEquals(it.next(), indIt.next());
		}
	}

	@Test
	public void testIteratorRemove() {
		final Iterator<OWLClassA> it = target.iterator();
		assertTrue(it.hasNext());
		it.next();
		it.remove();
		verify(uow).attributeChanged(owner, ownerField);
		assertEquals(backupSet.size() - 1, set.size());
	}

	@Test
	public void testAdd() {
		final OWLClassA a = new OWLClassA();
		a.setUri(URI.create("http://newA"));
		a.setStringAttribute("testAttribute");
		target.add(a);
		verify(uow).attributeChanged(owner, ownerField);
		assertEquals(backupSet.size() + 1, set.size());
	}

	@Test
	public void testRemove() {
		final OWLClassA toRemove = set.iterator().next();
		target.remove(toRemove);
		verify(uow).attributeChanged(owner, ownerField);
		assertEquals(backupSet.size() - 1, set.size());
	}

	@Test
	public void testAddAll() {
		final List<OWLClassA> toAdd = new ArrayList<OWLClassA>();
		for (int i = 0; i < 5; i++) {
			final OWLClassA a = new OWLClassA();
			a.setUri(URI.create("http://addAllA" + i));
			toAdd.add(a);
		}
		target.addAll(toAdd);
		verify(uow).attributeChanged(owner, ownerField);
		assertEquals(backupSet.size() + toAdd.size(), set.size());
		assertTrue(set.contains(toAdd.iterator().next()));
	}

	@Test
	public void testRetainAll() {
		Set<OWLClassA> toRetain = new HashSet<OWLClassA>();
		Iterator<OWLClassA> it = backupSet.iterator();
		for (int i = 0; i < 8; i++) {
			assertTrue(it.hasNext());
			toRetain.add(it.next());
		}
		target.retainAll(toRetain);
		verify(uow).attributeChanged(owner, ownerField);
		assertEquals(toRetain.size(), set.size());
		assertEquals(toRetain.size(), target.size());
	}

	@Test
	public void testRemoveAll() {
		Set<OWLClassA> toRemove = new HashSet<OWLClassA>();
		Iterator<OWLClassA> it = backupSet.iterator();
		for (int i = 0; i < 8; i++) {
			assertTrue(it.hasNext());
			toRemove.add(it.next());
		}
		target.removeAll(toRemove);
		verify(uow).attributeChanged(owner, ownerField);
		assertEquals(backupSet.size() - toRemove.size(), set.size());
	}

	@Test(expected = NullPointerException.class)
	public void testRemoveAllNull() {
		target.removeAll(null);
		fail("This line should not have been reached.");
	}

	@Test
	public void testClear() {
		target.clear();
		verify(uow).attributeChanged(owner, ownerField);
		assertTrue(set.isEmpty());
		assertTrue(target.isEmpty());
	}

}

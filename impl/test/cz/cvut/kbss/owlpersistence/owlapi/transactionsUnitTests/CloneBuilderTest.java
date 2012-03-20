package cz.cvut.kbss.owlpersistence.owlapi.transactionsUnitTests;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.logging.Logger;

import junit.framework.TestCase;

import org.junit.Before;
import org.junit.Test;

import cz.cvut.kbss.owlpersistence.owlapi.OWLClassA;
import cz.cvut.kbss.owlpersistence.owlapi.TestEnvironment;
import cz.cvut.kbss.owlpersistence.sessions.CloneBuilderImpl;
import cz.cvut.kbss.owlpersistence.sessions.ObjectChangeSet;
import cz.cvut.kbss.owlpersistence.sessions.UnitOfWorkChangeSet;
import cz.cvut.kbss.owlpersistence.sessions.UnitOfWorkChangeSetImpl;
import cz.cvut.kbss.owlpersistence.sessions.UnitOfWorkImpl;

public class CloneBuilderTest extends TestCase {

	private static Class<CloneBuilderImpl> cloneBuilderClass = CloneBuilderImpl.class;

	private Logger log = TestEnvironment.getLogger();
	CloneBuilderImpl builder;

	OWLClassA testEntity;

	List<OWLClassA> testEntities;

	@Before
	public void setUp() throws Exception {
		UnitOfWorkImpl uow = new UnitOfWorkImpl(null);
		this.builder = new CloneBuilderImpl(uow);
		this.testEntity = new OWLClassA();
		final URI pk = URI.create("http://testB");
		this.testEntity.setUri(pk);
		this.testEntity.setStringAttribute("TEST");
		Set<String> types = new HashSet<String>();
		types.add("testOne");
		types.add("testTwo");
		types.add("testThree");
		this.testEntity.setTypes(types);
		this.testEntities = new ArrayList<OWLClassA>();
		this.testEntities.add(testEntity);
		OWLClassA t2 = new OWLClassA();
		final URI pk2 = URI.create("http://testC");
		t2.setUri(pk2);
		t2.setStringAttribute("TEST2");
		this.testEntities.add(t2);
	}

	@Test
	public void testBuildClone() {
		OWLClassA res = (OWLClassA) this.builder.buildClone(this.testEntity);
		assertEquals(res.getStringAttribute(),
				this.testEntity.getStringAttribute());
		assertTrue(res.getUri().equals(this.testEntity.getUri()));
		assertEquals(testEntity.getTypes(), res.getTypes());
	}

	@SuppressWarnings("unchecked")
	@Test
	public void testBuildClones() {
		List<OWLClassA> result = (List<OWLClassA>) this.builder
				.buildClones(this.testEntities);
		assertEquals(this.testEntities.size(), result.size());
		int index = 0;
		for (OWLClassA ent : result) {
			assertEquals(this.testEntities.get(index).getUri(), ent.getUri());
			assertEquals(this.testEntities.get(index).getStringAttribute(),
					ent.getStringAttribute());
			index++;
		}
	}

	@Test
	public void testCreateObjectChangeSet() {
		UnitOfWorkChangeSet chSet = new UnitOfWorkChangeSetImpl(null);
		// We don't need to create some sort of special object, this is just
		// for the test
		Object clone = new Object();
		ObjectChangeSet res = this.builder.createObjectChangeSet(
				this.testEntity, clone, chSet);
		assertEquals(this.testEntity, res.getChangedObject());
		assertEquals(clone, res.getCloneObject());
	}

	@Test
	public void testCloneCollection() {
		final OWLClassA clone = (OWLClassA) builder.buildClone(testEntity);
		assertEquals(testEntity.getTypes().size(), clone.getTypes().size());
		Iterator<String> it1 = testEntity.getTypes().iterator();
		Iterator<String> it2 = clone.getTypes().iterator();
		while (it1.hasNext() && it2.hasNext()) {
			assertEquals(it1.next(), it2.next());
		}
	}
	
	@Test
	public void testCloneListCollection() {
		final List<String> testList = new ArrayList<String>();
		testList.add("One");
		testList.add("Two");
		testList.add("Three");
		@SuppressWarnings("unchecked")
		List<String> clone = (List<String>) builder.buildClone(testList);
		assertEquals(testList.size(), clone.size());
		Iterator<String> it1 = testList.iterator();
		Iterator<String> it2 = clone.iterator();
		while (it1.hasNext() && it2.hasNext()) {
			assertEquals(it1.next(), it2.next());
		}
	}

	@Test
	public void testCloneSingletonCollection() {
		final OWLClassA obj = new OWLClassA();
		final URI pk = URI.create("http://singletonTest");
		obj.setUri(pk);
		obj.setStringAttribute("TEST");
		String type = "A_type";
		obj.setTypes(Collections.singleton(type));
		try {
			final OWLClassA result =  (OWLClassA) builder.buildClone(obj);
			assertEquals(1, result.getTypes().size());
			assertEquals(type, result.getTypes().iterator().next());
		} catch (SecurityException e) {
			log.severe("Exception caught.");
			e.printStackTrace();
			fail();
		} catch (IllegalArgumentException e) {
			log.severe("Exception caught.");
			e.printStackTrace();
			fail();
		}
	}

	@Test
	public void testMergeChanges() {
		// TODO implement when the merging algorithm is implemented
	}

}

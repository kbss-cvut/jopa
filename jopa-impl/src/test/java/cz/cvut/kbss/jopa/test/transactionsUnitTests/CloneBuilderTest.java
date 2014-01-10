package cz.cvut.kbss.jopa.test.transactionsUnitTests;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Logger;

import org.junit.Before;
import org.junit.Test;

import cz.cvut.kbss.jopa.sessions.CloneBuilderImpl;
import cz.cvut.kbss.jopa.sessions.ObjectChangeSet;
import cz.cvut.kbss.jopa.sessions.UnitOfWorkChangeSet;
import cz.cvut.kbss.jopa.sessions.UnitOfWorkChangeSetImpl;
import cz.cvut.kbss.jopa.sessions.UnitOfWorkImpl;
import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.TestEnvironment;
import cz.cvut.kbss.jopa.test.utils.ServerSessionStub;

public class CloneBuilderTest {

	// private static Class<CloneBuilderImpl> cloneBuilderClass =
	// CloneBuilderImpl.class;

	private static final URI DEFAULT_URI = URI.create("http://defaultUri");

	private Logger log = TestEnvironment.getLogger();
	CloneBuilderImpl builder;

	OWLClassA testEntity;

	Map<OWLClassA, URI> testEntities;

	@Before
	public void setUp() throws Exception {
		UnitOfWorkImpl uow = new UnitOfWorkImpl(new ServerSessionStub());
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
		this.testEntities = new LinkedHashMap<OWLClassA, URI>();
		this.testEntities.put(testEntity, DEFAULT_URI);
		OWLClassA t2 = new OWLClassA();
		final URI pk2 = URI.create("http://testC");
		t2.setUri(pk2);
		t2.setStringAttribute("TEST2");
		this.testEntities.put(t2, DEFAULT_URI);
	}

	@Test
	public void testBuildClone() {
		OWLClassA res = (OWLClassA) this.builder.buildClone(this.testEntity, DEFAULT_URI);
		assertEquals(res.getStringAttribute(), this.testEntity.getStringAttribute());
		assertTrue(res.getUri().equals(this.testEntity.getUri()));
		assertEquals(testEntity.getTypes(), res.getTypes());
	}

	@Test(expected = NullPointerException.class)
	public void testBuildCloneNullOriginal() throws Exception {
		builder.buildClone(null, DEFAULT_URI);
		fail("This line should not have been reached.");
	}

	@Test(expected = NullPointerException.class)
	public void testBuildCloneNullContextUri() throws Exception {
		builder.buildClone(testEntity, null);
		fail("This line should not have been reached.");
	}

	@SuppressWarnings("unchecked")
	@Test
	public void testBuildClones() {
		List<OWLClassA> result = (List<OWLClassA>) this.builder.buildClones(this.testEntities);
		assertEquals(this.testEntities.size(), result.size());
		int index = 0;
		OWLClassA[] origs = new OWLClassA[testEntities.size()];
		origs = testEntities.keySet().toArray(origs);
		for (OWLClassA ent : result) {
			assertEquals(origs[index].getUri(), ent.getUri());
			assertEquals(origs[index].getStringAttribute(), ent.getStringAttribute());
			index++;
		}
	}

	@Test
	public void testCreateObjectChangeSet() {
		UnitOfWorkChangeSet chSet = new UnitOfWorkChangeSetImpl(null);
		// We don't need to create some sort of special object, this is just
		// for the test
		Object clone = new Object();
		ObjectChangeSet res = this.builder.createObjectChangeSet(this.testEntity, clone, chSet);
		assertEquals(this.testEntity, res.getChangedObject());
		assertEquals(clone, res.getCloneObject());
	}

	@Test
	public void testCloneCollection() {
		final OWLClassA clone = (OWLClassA) builder.buildClone(testEntity, DEFAULT_URI);
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
		List<String> clone = (List<String>) builder.buildClone(testList, DEFAULT_URI);
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
			final OWLClassA result = (OWLClassA) builder.buildClone(obj, DEFAULT_URI);
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

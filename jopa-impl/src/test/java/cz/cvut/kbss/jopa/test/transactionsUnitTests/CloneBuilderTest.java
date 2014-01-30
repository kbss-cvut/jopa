package cz.cvut.kbss.jopa.test.transactionsUnitTests;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.logging.Logger;

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import cz.cvut.kbss.jopa.adapters.IndirectCollection;
import cz.cvut.kbss.jopa.sessions.CloneBuilderImpl;
import cz.cvut.kbss.jopa.sessions.ObjectChangeSet;
import cz.cvut.kbss.jopa.sessions.UnitOfWorkChangeSet;
import cz.cvut.kbss.jopa.sessions.UnitOfWorkChangeSetImpl;
import cz.cvut.kbss.jopa.sessions.UnitOfWorkImpl;
import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.OWLClassB;
import cz.cvut.kbss.jopa.test.OWLClassC;
import cz.cvut.kbss.jopa.test.OWLClassD;
import cz.cvut.kbss.jopa.test.TestEnvironment;
import cz.cvut.kbss.jopa.test.utils.Generators;
import cz.cvut.kbss.jopa.test.utils.ServerSessionStub;

public class CloneBuilderTest {

	private static final URI DEFAULT_URI = URI.create("http://defaultUri");

	private static Logger log = TestEnvironment.getLogger();

	CloneBuilderImpl builder;

	private static OWLClassA entityA;
	private static OWLClassB entityB;
	private static OWLClassC entityC;
	private static OWLClassD entityD;
	private static Set<String> types;

	private static Map<OWLClassA, URI> testEntities;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		entityA = new OWLClassA();
		final URI pk = URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityA");
		entityA.setUri(pk);
		entityA.setStringAttribute("TEST");
		types = new HashSet<String>();
		types.add("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityA");
		types.add("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityU");
		types.add("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityX");
		entityA.setTypes(types);
		testEntities = new LinkedHashMap<OWLClassA, URI>();
		testEntities.put(entityA, DEFAULT_URI);
		OWLClassA t2 = new OWLClassA();
		final URI pk2 = URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityAA");
		t2.setUri(pk2);
		t2.setStringAttribute("TEST2");
		testEntities.put(t2, DEFAULT_URI);
		entityB = new OWLClassB();
		entityB.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityB"));
		entityB.setStringAttribute("someString");
		entityC = new OWLClassC();
		entityC.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityC"));
		entityD = new OWLClassD();
		entityD.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityD"));
		entityD.setOwlClassA(entityA);
	}

	@Before
	public void setUp() throws Exception {
		UnitOfWorkImpl uow = new UnitOfWorkImpl(new ServerSessionStub());
		this.builder = new CloneBuilderImpl(uow);
		entityA.setTypes(types);
		entityB.setProperties(null);
		entityC.setReferencedList(null);
		entityC.setSimpleList(null);
	}

	@Test
	public void testBuildClone() {
		OWLClassA res = (OWLClassA) this.builder.buildClone(entityA, DEFAULT_URI);
		assertEquals(res.getStringAttribute(), entityA.getStringAttribute());
		assertTrue(res.getUri().equals(entityA.getUri()));
		assertEquals(entityA.getTypes(), res.getTypes());
	}

	@Test(expected = NullPointerException.class)
	public void testBuildCloneNullOriginal() throws Exception {
		builder.buildClone(null, DEFAULT_URI);
		fail("This line should not have been reached.");
	}

	@Test(expected = NullPointerException.class)
	public void testBuildCloneNullContextUri() throws Exception {
		builder.buildClone(entityA, null);
		fail("This line should not have been reached.");
	}

	@SuppressWarnings("unchecked")
	@Test
	public void testBuildClones() {
		List<OWLClassA> result = (List<OWLClassA>) this.builder.buildClones(testEntities);
		assertEquals(testEntities.size(), result.size());
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
		ObjectChangeSet res = this.builder.createObjectChangeSet(entityA, clone, chSet);
		assertEquals(entityA, res.getChangedObject());
		assertEquals(clone, res.getCloneObject());
	}

	@Test
	public void testCloneCollection() {
		final OWLClassA clone = (OWLClassA) builder.buildClone(entityA, DEFAULT_URI);
		assertEquals(entityA.getTypes().size(), clone.getTypes().size());
		for (String t : entityA.getTypes()) {
			assertTrue(clone.getTypes().contains(t));
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
	public void testCloneProperties() {
		entityB.setProperties(createProperties());
		OWLClassB res = (OWLClassB) builder.buildClone(entityB, DEFAULT_URI);
		assertNotNull(res);
		assertEquals(entityB.getUri(), res.getUri());
		assertEquals(entityB.getStringAttribute(), res.getStringAttribute());
		assertEquals(entityB.getProperties().size(), res.getProperties().size());
		assertTrue(res.getProperties() instanceof IndirectCollection);
		for (Entry<String, Set<String>> e : entityB.getProperties().entrySet()) {
			final String k = e.getKey();
			assertTrue(res.getProperties().containsKey(k));
			final Set<String> rv = res.getProperties().get(k);
			assertTrue(rv instanceof IndirectCollection);
			assertEquals(e.getValue().size(), rv.size());
			for (String s : e.getValue()) {
				assertTrue(rv.contains(s));
			}
		}
	}

	@Test
	public void testCloneObjectProperty() {
		final OWLClassD another = new OWLClassD();
		another.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityDD"));
		another.setOwlClassA(entityA);
		final OWLClassD clOne = (OWLClassD) builder.buildClone(entityD, DEFAULT_URI);
		assertFalse(clOne == entityD);
		assertFalse(clOne.getOwlClassA() == entityA);
		final OWLClassD clTwo = (OWLClassD) builder.buildClone(another, DEFAULT_URI);
		assertTrue(clOne.getOwlClassA() == clTwo.getOwlClassA());
		assertEquals(entityA.getStringAttribute(), clOne.getOwlClassA().getStringAttribute());
		assertTrue(clOne.getOwlClassA().getTypes() instanceof IndirectCollection);
		final Set<String> tps = clOne.getOwlClassA().getTypes();
		assertEquals(entityA.getTypes().size(), tps.size());
		for (String t : entityA.getTypes()) {
			assertTrue(tps.contains(t));
		}
	}

	@Test
	public void testCloneWithNullCollection() {
		assertNull(entityB.getProperties());
		final OWLClassB res = (OWLClassB) builder.buildClone(entityB, DEFAULT_URI);
		assertFalse(entityB == res);
		assertNull(res.getProperties());
	}

	@Test
	public void testCloneSingletonSet() {
		final Set<String> singleton = Collections
				.singleton("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityY");
		entityA.setTypes(singleton);
		final OWLClassA res = (OWLClassA) builder.buildClone(entityA, DEFAULT_URI);
		assertFalse(entityA == res);
		assertTrue(res.getTypes() instanceof IndirectCollection);
		assertEquals(1, res.getTypes().size());
		assertTrue(res.getTypes().contains(singleton.iterator().next()));
	}

	@Test
	public void testCloneSingletonMap() {
		final String key = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#attr";
		final String value = "stringValue";
		final Map<String, Set<String>> m = Collections.singletonMap(key,
				Collections.singleton(value));
		entityB.setProperties(m);
		final OWLClassB res = (OWLClassB) builder.buildClone(entityB, DEFAULT_URI);
		assertNotNull(res);
		assertFalse(entityB == res);
		assertEquals(1, res.getProperties().size());
		assertTrue(res.getProperties() instanceof IndirectCollection);
		final Set<String> s = res.getProperties().get(key);
		assertTrue(s instanceof IndirectCollection);
		assertEquals(1, s.size());
		assertEquals(value, s.iterator().next());
	}

	@Test
	public void testCloneSingletonListWithReference() {
		entityC.setReferencedList(Collections.singletonList(entityA));
		final OWLClassC res = (OWLClassC) builder.buildClone(entityC, DEFAULT_URI);
		assertFalse(res == entityC);
		assertEquals(1, res.getReferencedList().size());
		assertTrue(res.getReferencedList() instanceof IndirectCollection);
		final OWLClassA a = res.getReferencedList().get(0);
		assertFalse(entityA == a);
		assertEquals(entityA.getUri(), a.getUri());
		assertTrue(a.getTypes() instanceof IndirectCollection);
	}

	@Test
	public void testCloneReferencedList() {
		// Let's see how long this takes
		entityC.setReferencedList(Generators.createReferencedList(100));
		final OWLClassC res = (OWLClassC) builder.buildClone(entityC, DEFAULT_URI);
		assertFalse(entityC == res);
		int size = entityC.getReferencedList().size();
		assertEquals(size, res.getReferencedList().size());
		assertTrue(res.getReferencedList() instanceof IndirectCollection);
		for (int i = 0; i < size; i++) {
			final OWLClassA or = entityC.getReferencedList().get(i);
			final OWLClassA cl = res.getReferencedList().get(i);
			assertFalse(or == cl);
			assertEquals(or.getUri(), cl.getUri());
			assertEquals(or.getStringAttribute(), cl.getStringAttribute());
			assertEquals(or.getTypes().size(), cl.getTypes().size());
			assertTrue(cl.getTypes() instanceof IndirectCollection);
		}
	}

	private static Map<String, Set<String>> createProperties() {
		final Map<String, Set<String>> m = new HashMap<>(5);
		for (int i = 0; i < 5; i++) {
			final String key = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#attr" + i;
			final String value = "http://krizik.felk.cvut.cz/ontologies/jopa/tests/value" + i;
			final Set<String> vals = new HashSet<>(1);
			vals.add(value);
			m.put(key, vals);
		}
		return m;
	}
}

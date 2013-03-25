package cz.cvut.kbss.jopa.owlapi.transactionsUnitTests;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.net.URI;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import cz.cvut.kbss.jopa.model.OWLInferredAttributeModifiedException;
import cz.cvut.kbss.jopa.owlapi.OWLClassA;
import cz.cvut.kbss.jopa.owlapi.OWLClassC;
import cz.cvut.kbss.jopa.owlapi.OWLClassD;
import cz.cvut.kbss.jopa.owlapi.utils.ServerSessionStub;
import cz.cvut.kbss.jopa.sessions.ChangeManager;
import cz.cvut.kbss.jopa.sessions.ChangeManagerImpl;
import cz.cvut.kbss.jopa.sessions.ChangeRecord;
import cz.cvut.kbss.jopa.sessions.CloneBuilder;
import cz.cvut.kbss.jopa.sessions.CloneBuilderImpl;
import cz.cvut.kbss.jopa.sessions.ObjectChangeSet;
import cz.cvut.kbss.jopa.sessions.UnitOfWorkImpl;

public class ChangeManagerTest {

	private static final URI DEFAULT_CONTEXT = URI.create("http://defaultContext");
	private static ChangeManager manager;
	private static CloneBuilder builder;
	private static OWLClassA testA;
	private static OWLClassD testD;
	private static OWLClassC testC;
	private static OWLClassA testAClone;
	private static OWLClassD testDClone;
	private static OWLClassC testCClone;
	private static Set<String> typesCollection;
	private static TestEntity primitivesTest;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		manager = new ChangeManagerImpl();
		builder = new CloneBuilderImpl(new UnitOfWorkImpl(new ServerSessionStub()));
		testA = new OWLClassA();
		final URI uri = URI.create("http://testA");
		testA.setUri(uri);
		testA.setStringAttribute("TestStringAttribute");
		final URI uri2 = URI.create("http://referencedA");
		final OWLClassA refA = new OWLClassA();
		refA.setUri(uri2);
		refA.setStringAttribute("att");
		testD = new OWLClassD();
		final URI uri3 = URI.create("http://testD");
		testD.setUri(uri3);
		testD.setOwlClassA(refA);
		testAClone = new OWLClassA();
		testAClone.setUri(uri);
		testDClone = new OWLClassD();
		testDClone.setUri(uri3);
		typesCollection = new HashSet<String>();
		int i;
		for (i = 0; i < 10; i++) {
			typesCollection.add(Integer.toString(i));
		}
		testC = new OWLClassC();
		final URI pkC = URI.create("http://testC");
		testC.setUri(pkC);
		List<OWLClassA> refList = new ArrayList<OWLClassA>(10);
		for (i = 0; i < 10; i++) {
			OWLClassA a = new OWLClassA();
			URI pkA = URI.create("http://testARef" + (i + 1));
			a.setUri(pkA);
			a.setStringAttribute("stringForA" + (i + 1));
			refList.add(a);
		}
		testC.setReferencedList(refList);
		primitivesTest = new TestEntity();
		primitivesTest.setId(0);
	}

	@Before
	public void setup() {
		builder.reset();
		testAClone.setStringAttribute(null);
		testAClone.setTypes(null);
		testA.setTypes(null);
		testCClone = null;
		testCClone = (OWLClassC) builder.buildClone(testC, DEFAULT_CONTEXT);
	}

	@Test
	public void testNullHasChanges() {
		assertFalse(manager.hasChanges(null, null));
	}

	@Test
	public void testHasSimpleChanges() {
		testAClone.setStringAttribute("differentStringAtribute");
		assertTrue(manager.hasChanges(testA, testAClone));
	}

	@Test
	public void testHasNoChanges() {
		testAClone.setStringAttribute(testA.getStringAttribute());
		assertFalse(manager.hasChanges(testA, testA));
	}

	@Test
	public void testReferenceChange() {
		final OWLClassA ref = new OWLClassA();
		final URI uri = URI.create("http://newReferenceA");
		ref.setUri(uri);
		ref.setStringAttribute(testA.getStringAttribute());
		testDClone.setOwlClassA(ref);
		assertTrue(manager.hasChanges(testD, testDClone));
	}

	@Test
	public void testCollectionEmptyHasChange() {
		testAClone.setTypes(typesCollection);
		testA.setTypes(new HashSet<String>());
		assertTrue(manager.hasChanges(testA, testAClone));
	}

	@Test
	public void testCollectionHasChange() {
		testA.setTypes(typesCollection);
		Set<String> changed = new HashSet<String>();
		Iterator<String> it = typesCollection.iterator();
		it.next();
		changed.add("111");
		while (it.hasNext()) {
			changed.add(it.next());
		}
		testAClone.setTypes(changed);
		assertTrue(manager.hasChanges(testA, testAClone));
	}

	@Test
	public void testCollectionComplexHasChange() {
		assertEquals(testC.getUri(), testCClone.getUri());
		assertEquals(testC.getReferencedList().get(0).getUri(),
				testCClone.getReferencedList().get(0).getUri());
		assertEquals(testC.getReferencedList().get(5).getStringAttribute(), testCClone
				.getReferencedList().get(5).getStringAttribute());
		testCClone.getReferencedList().get(8).setStringAttribute("changedStringAttribute");
		assertTrue(manager.hasChanges(testC, testCClone));
	}

	@Test
	public void testCalculateSimpleChanges() throws IllegalAccessException,
			IllegalArgumentException, OWLInferredAttributeModifiedException {
		ObjectChangeSet chSet = builder.createObjectChangeSet(testA, testAClone, null);
		chSet = manager.calculateChanges(chSet);
		assertNotNull(chSet);
		assertFalse(0 == chSet.getChanges().size());
		assertTrue(chSet.getAttributesToChange().containsKey("stringAttribute"));
	}

	@Test
	public void testCalcuateChangesPrimitives() throws IllegalAccessException,
			IllegalArgumentException, OWLInferredAttributeModifiedException {
		final TestEntity primClone = new TestEntity();
		primClone.setId(primitivesTest.getId() + 10);
		ObjectChangeSet chSet = builder.createObjectChangeSet(primitivesTest, primClone, null);
		chSet = manager.calculateChanges(chSet);
		assertNotNull(chSet);
		assertFalse(0 == chSet.getChanges().size());
		assertTrue(chSet.getAttributesToChange().containsKey("id"));
	}

	@Test
	public void testCalculateReferenceChanges() throws IllegalAccessException,
			IllegalArgumentException, OWLInferredAttributeModifiedException {
		testDClone.setOwlClassA(testAClone);
		ObjectChangeSet chSet = builder.createObjectChangeSet(testD, testDClone, null);
		chSet = manager.calculateChanges(chSet);
		assertNotNull(chSet);
		List<ChangeRecord> records = chSet.getChanges();
		assertEquals(1, records.size());
		assertEquals(testAClone, records.get(0).getNewValue());
	}

	@SuppressWarnings("unchecked")
	@Test
	public void testCalculateCollectionChanges() throws IllegalAccessException,
			IllegalArgumentException, OWLInferredAttributeModifiedException {
		testA.setTypes(typesCollection);
		Set<String> newCollection = new HashSet<String>(typesCollection);
		newCollection.remove("8");
		newCollection.add("String");
		testAClone.setTypes(newCollection);
		testAClone.setStringAttribute(testA.getStringAttribute());
		ObjectChangeSet chSet = builder.createObjectChangeSet(testA, testAClone, null);
		chSet = manager.calculateChanges(chSet);
		assertNotNull(chSet);
		List<ChangeRecord> records = chSet.getChanges();
		assertEquals(1, records.size());
		assertTrue(((Set<String>) records.get(0).getNewValue()).contains("String"));
	}

	@Test
	public void testCalculateMultipleChanges() throws IllegalAccessException,
			IllegalArgumentException, OWLInferredAttributeModifiedException {
		testA.setTypes(typesCollection);
		Set<String> newCollection = new HashSet<String>(typesCollection);
		newCollection.remove("8");
		newCollection.add("String");
		testAClone.setTypes(newCollection);
		final URI newURI = URI.create("http://newACloneURI");
		testAClone.setUri(newURI);
		testAClone.setStringAttribute("AnotherStringAttribute");
		ObjectChangeSet chSet = builder.createObjectChangeSet(testA, testAClone, null);
		chSet = manager.calculateChanges(chSet);
		assertNotNull(chSet);
		List<ChangeRecord> records = chSet.getChanges();
		assertEquals(3, records.size());
		assertTrue(chSet.getAttributesToChange().containsKey("stringAttribute"));
		assertTrue(chSet.getAttributesToChange().containsKey("types"));
		assertTrue(chSet.getAttributesToChange().containsKey("uri"));
	}

	@Test
	public void testCalculateChangesSetNull() throws IllegalAccessException,
			IllegalArgumentException, OWLInferredAttributeModifiedException {
		ObjectChangeSet chSet = builder.createObjectChangeSet(testA, testAClone, null);
		chSet = manager.calculateChanges(chSet);
		assertNotNull(chSet);
		assertNull(chSet.getAttributesToChange().get("stringAttribute").getNewValue());
	}

	@Test
	public void testCalculateChangesRemoveItemFromReferenceList() throws IllegalAccessException,
			IllegalArgumentException, OWLInferredAttributeModifiedException {
		testCClone.getReferencedList().remove(4);
		ObjectChangeSet chSet = builder.createObjectChangeSet(testC, testCClone, null);
		chSet = manager.calculateChanges(chSet);
		assertNotNull(chSet);
		assertTrue(chSet.getAttributesToChange().containsKey("referencedList"));
		assertEquals(1, chSet.getChanges().size());
	}

	@SuppressWarnings("unchecked")
	@Test
	public void testCalculateChangesAddItemtoReferenceList() throws IllegalAccessException,
			IllegalArgumentException, OWLInferredAttributeModifiedException {
		OWLClassA add = new OWLClassA();
		final URI pk = URI.create("http://addedA");
		add.setUri(pk);
		add.setStringAttribute("string");
		testCClone.getReferencedList().add(add);
		ObjectChangeSet chSet = builder.createObjectChangeSet(testC, testCClone, null);
		chSet = manager.calculateChanges(chSet);
		assertNotNull(chSet);
		assertEquals(1, chSet.getChanges().size());
		final ChangeRecord r = chSet.getChanges().get(0);
		List<OWLClassA> refs = (List<OWLClassA>) r.getNewValue();
		assertEquals(add.getUri(), refs.get(10).getUri());
		assertEquals(add.getStringAttribute(), refs.get(10).getStringAttribute());
	}

	@SuppressWarnings("unchecked")
	public void testCalculateChangesChangeItemInReferenceList() throws IllegalAccessException,
			IllegalArgumentException, OWLInferredAttributeModifiedException {
		OWLClassA newOne = new OWLClassA();
		final URI pk = URI.create("http://newOne");
		newOne.setUri(pk);
		newOne.setStringAttribute("newOnesString");
		testCClone.getReferencedList().remove(3);
		testCClone.getReferencedList().add(newOne);
		ObjectChangeSet chSet = builder.createObjectChangeSet(testC, testCClone, null);
		chSet = manager.calculateChanges(chSet);
		assertNotNull(chSet);
		assertTrue(chSet.getAttributesToChange().containsKey("referencedList"));
		assertEquals(1, chSet.getChanges().size());
		final ChangeRecord r = chSet.getChanges().get(0);
		List<OWLClassA> refs = (List<OWLClassA>) r.getNewValue();
		assertTrue(refs.contains(newOne));
	}

	private static final class TestEntity {

		private int id;

		public int getId() {
			return id;
		}

		public void setId(int id) {
			this.id = id;
		}

	}
}

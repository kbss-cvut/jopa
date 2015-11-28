package cz.cvut.kbss.jopa.sessions.change;

import cz.cvut.kbss.jopa.environment.*;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.environment.utils.TestEnvironmentUtils;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.Identifier;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.sessions.ChangeManager;
import cz.cvut.kbss.jopa.sessions.ChangeRecord;
import cz.cvut.kbss.jopa.sessions.MetamodelProvider;
import cz.cvut.kbss.jopa.sessions.ObjectChangeSet;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.net.URI;
import java.util.*;
import java.util.Map.Entry;
import java.util.stream.Collectors;

import static org.junit.Assert.*;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class ChangeManagerTest {

    private static final URI DEFAULT_CONTEXT = URI.create("http://defaultContext");

    private static OWLClassA testA;
    private static OWLClassD testD;
    private static OWLClassC testC;
    private static OWLClassA testAClone;
    private static OWLClassD testDClone;
    private OWLClassC testCClone;
    private static OWLClassB testB;
    private static OWLClassB testBClone;
    private static Set<String> typesCollection;
    private static TestEntity primitivesTest;
    private static Descriptor defaultDescriptor;

    @Mock
    private MetamodelProvider providerMock;
    @Mock
    private Metamodel metamodelMock;

    private ChangeManager manager;

    @BeforeClass
    public static void setUpBeforeClass() throws Exception {
        initAs();
        initBs();
        initC();
        initDs();
        typesCollection = new HashSet<>();
        for (int i = 0; i < 10; i++) {
            typesCollection.add(Integer.toString(i));
        }
        OWLClassF testF = new OWLClassF();
        testF.setUri(URI.create("http://testF"));
        primitivesTest = new TestEntity();
        primitivesTest.setId(0);
        defaultDescriptor = new EntityDescriptor(DEFAULT_CONTEXT);
    }

    private static void initAs() {
        testA = new OWLClassA();
        final URI uri = URI.create("http://testA");
        testA.setUri(uri);
        testA.setStringAttribute("TestStringAttribute");
        testAClone = new OWLClassA();
        testAClone.setUri(uri);
    }

    private static void initBs() {
        testB = new OWLClassB();
        testB.setUri(URI.create("http://testB"));
        testB.setStringAttribute("someString");
        final Map<String, Set<String>> props = new HashMap<>();
        props.put("propertyOne", Collections.singleton("valueOne"));
        props.put("propertyTwo", Collections.singleton("valueTwo"));
        final Set<String> multiProp = new HashSet<>();
        multiProp.add("valueThree");
        multiProp.add("valueFour");
        props.put("propertyThree", multiProp);
        testB.setProperties(props);
        testBClone = new OWLClassB();
        testBClone.setUri(testB.getUri());
        testBClone.setStringAttribute(testB.getStringAttribute());
    }

    private static void initC() {
        testC = new OWLClassC();
        final URI pkC = URI.create("http://testC");
        testC.setUri(pkC);
        List<OWLClassA> refList = new ArrayList<>(10);
        for (int i = 0; i < 10; i++) {
            OWLClassA a = new OWLClassA();
            URI pkA = URI.create("http://testARef" + (i + 1));
            a.setUri(pkA);
            a.setStringAttribute("stringForA" + (i + 1));
            refList.add(a);
        }
        testC.setReferencedList(refList);
    }

    private static void initDs() {
        final URI uri2 = URI.create("http://referencedA");
        final OWLClassA refA = new OWLClassA();
        refA.setUri(uri2);
        refA.setStringAttribute("att");
        testD = new OWLClassD();
        final URI uri3 = URI.create("http://testD");
        testD.setUri(uri3);
        testD.setOwlClassA(refA);
        testDClone = new OWLClassD();
        testDClone.setUri(uri3);
    }

    @Before
    public void setup() throws Exception {
        MockitoAnnotations.initMocks(this);
        manager = new ChangeManagerImpl(providerMock);
        when(providerMock.isTypeManaged(any(Class.class))).thenAnswer(invocation -> {
            final Class<?> cls = (Class<?>) invocation.getArguments()[0];
            return TestEnvironmentUtils.getManagedTypes().contains(cls);
        });
        when(providerMock.getMetamodel()).thenReturn(metamodelMock);
        final MetamodelMocks mocks = new MetamodelMocks();
        mocks.setMocks(metamodelMock);
        testAClone.setStringAttribute(null);
        testAClone.setTypes(null);
        testA.setTypes(null);
        final Map<String, Set<String>> cloneProps = new HashMap<>();
        for (Entry<String, Set<String>> e : testB.getProperties().entrySet()) {
            final Set<String> set = e.getValue().stream().collect(Collectors.toSet());
            cloneProps.put(e.getKey(), set);
        }
        testBClone.setProperties(cloneProps);
        testCClone = new OWLClassC();
        testCClone.setUri(testC.getUri());
        final List<OWLClassA> clonedList = new ArrayList<>(testC.getReferencedList().size());
        for (OWLClassA a : testC.getReferencedList()) {
            final OWLClassA newA = new OWLClassA();
            newA.setUri(a.getUri());
            newA.setStringAttribute(a.getStringAttribute());
            clonedList.add(newA);
        }
        testCClone.setReferencedList(clonedList);
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
        testA.setTypes(new HashSet<>());
        assertTrue(manager.hasChanges(testA, testAClone));
    }

    @Test
    public void testCollectionHasChange() {
        testA.setTypes(typesCollection);
        Set<String> changed = new HashSet<>();
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
    public void testHasChangeSetFromNull() {
        final OWLClassA a = new OWLClassA();
        a.setUri(testAClone.getUri());
        // The original has string attribute null
        testAClone.setStringAttribute("someString");
        assertTrue(manager.hasChanges(a, testAClone));
    }

    @Test(expected = NullPointerException.class)
    public void testCalculateChangesNull() throws Exception {
        manager.calculateChanges(null);
        fail("This line should not have been reached.");
    }

    @Test
    public void testCalculateChangesNoChanges() throws Exception {
        final OWLClassA a = new OWLClassA();
        a.setUri(testA.getUri());
        a.setStringAttribute(testA.getStringAttribute());
        final ObjectChangeSet chSet = createChangeSet(testA, a);
        final boolean res = manager.calculateChanges(chSet);
        assertFalse(res);
        assertTrue(chSet.getChanges().isEmpty());
    }

    @Test
    public void testCalculateSimpleChanges() throws Exception {
        ObjectChangeSet chSet = createChangeSet(testA, testAClone);
        assertTrue(chSet.getChanges().isEmpty());
        final boolean res = manager.calculateChanges(chSet);
        assertTrue(res);
        assertFalse(chSet.getChanges().isEmpty());
        assertTrue(chSet.getChanges().containsKey("stringAttribute"));
    }

    @Test
    public void testCalculateChangesPrimitives() throws Exception {
        final TestEntity primClone = new TestEntity();
        primClone.setId(primitivesTest.getId() + 10);
        ObjectChangeSet chSet = createChangeSet(primitivesTest, primClone);
        assertTrue(chSet.getChanges().isEmpty());
        final boolean res = manager.calculateChanges(chSet);
        assertTrue(res);
        assertFalse(chSet.getChanges().isEmpty());
        assertTrue(chSet.getChanges().containsKey("id"));
    }

    @Test
    public void testCalculateReferenceChanges() throws Exception {
        testDClone.setOwlClassA(testAClone);
        ObjectChangeSet chSet = createChangeSet(testD, testDClone);
        assertTrue(chSet.getChanges().isEmpty());
        final boolean res = manager.calculateChanges(chSet);
        assertTrue(res);
        assertEquals(1, chSet.getChanges().size());
        assertEquals(testAClone, chSet.getChanges().get(OWLClassD.getOwlClassAField().getName()).getNewValue());
    }

    @SuppressWarnings("unchecked")
    @Test
    public void testCalculateCollectionChanges() throws Exception {
        testA.setTypes(typesCollection);
        Set<String> newCollection = new HashSet<>(typesCollection);
        newCollection.remove("8");
        newCollection.add("String");
        testAClone.setTypes(newCollection);
        testAClone.setStringAttribute(testA.getStringAttribute());
        ObjectChangeSet chSet = createChangeSet(testA, testAClone);
        assertTrue(chSet.getChanges().isEmpty());
        final boolean res = manager.calculateChanges(chSet);
        assertTrue(res);
        assertEquals(1, chSet.getChanges().size());
        assertTrue(((Set<String>) chSet.getChanges().
                get(OWLClassA.getTypesField().getName()).getNewValue()).contains("String"));
    }

    @Test
    public void testCalculateMultipleChanges() throws Exception {
        testA.setTypes(typesCollection);
        Set<String> newCollection = new HashSet<>(typesCollection);
        newCollection.remove("8");
        newCollection.add("String");
        testAClone.setTypes(newCollection);
        final URI newURI = URI.create("http://newACloneURI");
        testAClone.setUri(newURI);
        testAClone.setStringAttribute("AnotherStringAttribute");
        ObjectChangeSet chSet = createChangeSet(testA, testAClone);
        assertTrue(chSet.getChanges().isEmpty());
        final boolean res = manager.calculateChanges(chSet);
        assertTrue(res);
        assertEquals(3, chSet.getChanges().size());
        assertTrue(chSet.getChanges().containsKey("stringAttribute"));
        assertTrue(chSet.getChanges().containsKey("types"));
        assertTrue(chSet.getChanges().containsKey("uri"));
    }

    @Test
    public void testCalculateChangesSetNull() throws Exception {
        ObjectChangeSet chSet = createChangeSet(testA, testAClone);
        assertTrue(chSet.getChanges().isEmpty());
        final boolean res = manager.calculateChanges(chSet);
        assertTrue(res);
        assertFalse(chSet.getChanges().isEmpty());
        assertNotNull(chSet);
        assertNull(chSet.getChanges().get("stringAttribute").getNewValue());
    }

    @Test
    public void testCalculateChangeSetFromNull() throws Exception {
        final OWLClassA a = new OWLClassA();
        a.setUri(testAClone.getUri());
        // The original has string attribute null
        testAClone.setStringAttribute("someString");
        final ObjectChangeSet chSet = createChangeSet(a, testAClone);
        final boolean res = manager.calculateChanges(chSet);
        assertTrue(res);
        assertEquals(1, chSet.getChanges().size());
        assertEquals(1, chSet.getChanges().size());
        final ChangeRecord rec = chSet.getChanges().get(OWLClassA.getStrAttField().getName());
        assertNotNull(rec.getNewValue());
    }

    @Test
    public void testCalculateChangesRemoveItemFromReferenceList() throws Exception {
        testCClone.getReferencedList().remove(4);
        ObjectChangeSet chSet = createChangeSet(testC, testCClone);
        assertTrue(chSet.getChanges().isEmpty());
        final boolean res = manager.calculateChanges(chSet);
        assertTrue(res);
        assertTrue(chSet.getChanges().containsKey("referencedList"));
        assertEquals(1, chSet.getChanges().size());
    }

    @SuppressWarnings("unchecked")
    @Test
    public void testCalculateChangesAddItemToReferenceList() throws Exception {
        OWLClassA add = new OWLClassA();
        final URI pk = URI.create("http://addedA");
        add.setUri(pk);
        add.setStringAttribute("string");
        testCClone.getReferencedList().add(add);
        ObjectChangeSet chSet = createChangeSet(testC, testCClone);
        assertTrue(chSet.getChanges().isEmpty());
        final boolean res = manager.calculateChanges(chSet);
        assertTrue(res);
        assertEquals(1, chSet.getChanges().size());
        final ChangeRecord r = chSet.getChanges().get(OWLClassC.getRefListField().getName());
        List<OWLClassA> refs = (List<OWLClassA>) r.getNewValue();
        assertEquals(add.getUri(), refs.get(10).getUri());
        assertEquals(add.getStringAttribute(), refs.get(10).getStringAttribute());
    }

    @SuppressWarnings("unchecked")
    @Test
    public void testCalculateChangesChangeItemInReferenceList() throws Exception {
        OWLClassA newOne = new OWLClassA();
        final URI pk = URI.create("http://newOne");
        newOne.setUri(pk);
        newOne.setStringAttribute("newOnesString");
        testCClone.getReferencedList().remove(3);
        testCClone.getReferencedList().add(newOne);
        ObjectChangeSet chSet = createChangeSet(testC, testCClone);
        assertTrue(chSet.getChanges().isEmpty());
        final boolean res = manager.calculateChanges(chSet);
        assertTrue(res);
        assertTrue(chSet.getChanges().containsKey("referencedList"));
        assertEquals(1, chSet.getChanges().size());
        final ChangeRecord r = chSet.getChanges().get(OWLClassC.getRefListField().getName());
        List<OWLClassA> refs = (List<OWLClassA>) r.getNewValue();
        assertTrue(refs.contains(newOne));
    }

    @Test
    public void testCalculateChangesMapAddKey() throws Exception {
        testBClone.getProperties().put("newProperty", Collections.singleton("propVal"));
        final ObjectChangeSet chSet = createChangeSet(testB, testBClone);
        final boolean res = manager.calculateChanges(chSet);
        assertTrue(res);
        assertEquals(1, chSet.getChanges().size());
        assertTrue(chSet.getChanges().containsKey("properties"));
    }

    @Test
    public void testCalculateChangesMapChangeValue() throws Exception {
        final String key = testB.getProperties().keySet().iterator().next();
        testBClone.getProperties().put(key, Collections.singleton("propVal"));
        final ObjectChangeSet chSet = createChangeSet(testB, testBClone);
        final boolean res = manager.calculateChanges(chSet);
        assertTrue(res);
        assertEquals(1, chSet.getChanges().size());
        assertTrue(chSet.getChanges().containsKey("properties"));
    }

    @Test
    public void testCalculateChangesMapAddValue() throws Exception {
        final String key = testB.getProperties().keySet().iterator().next();
        testBClone.getProperties().get(key).add("addedValue");
        final ObjectChangeSet chSet = createChangeSet(testB, testBClone);
        final boolean res = manager.calculateChanges(chSet);
        assertTrue(res);
        assertEquals(1, chSet.getChanges().size());
        assertTrue(chSet.getChanges().containsKey("properties"));
    }

    private ObjectChangeSet createChangeSet(Object orig, Object clone) {
        return TestEnvironmentUtils.createObjectChangeSet(orig, clone,
                defaultDescriptor.getContext());
    }

    @Test
    public void twoSetsWithSameElementsButDifferentOrderHaveNoChanges() throws Exception {
        testA.setTypes(typesCollection);
        testAClone.setStringAttribute(testA.getStringAttribute());
        final List<String> lst = new ArrayList<>(typesCollection);
        Collections.reverse(lst);
        final Set<String> newTypes = new LinkedHashSet<>(lst);
        testAClone.setTypes(newTypes);
        assertFalse(typesCollection.iterator().next().equals(newTypes.iterator().next()));
        final boolean res = manager.hasChanges(testA, testAClone);
        assertFalse(res);
    }

    @Test
    public void twoSetsWithManagedElementsWithSameIdentifiersHaveNoChanges() throws Exception {
        final OWLClassF testF = new OWLClassF();
        final OWLClassF cloneF = new OWLClassF();
        initFAndClone(testF, cloneF);
        initMetamodelForClassA();
        final boolean res = manager.hasChanges(testF, cloneF);
        assertFalse(res);
    }

    @SuppressWarnings("unchecked")
    private void initMetamodelForClassA() throws NoSuchFieldException {
        final EntityType<OWLClassA> etAMock = mock(EntityType.class);
        final Identifier idMock = mock(Identifier.class);
        when(idMock.getJavaField()).thenReturn(OWLClassA.class.getDeclaredField("uri"));
        when(etAMock.getIdentifier()).thenReturn(idMock);
        when(metamodelMock.entity(OWLClassA.class)).thenReturn(etAMock);
    }

    private void initFAndClone(OWLClassF orig, OWLClassF clone) {
        final URI uri = URI.create("http://krizik.felk.cvut.cz/ontologies#testF");
        orig.setUri(uri);
        clone.setUri(uri);
        orig.setSimpleSet(new HashSet<>());
        clone.setSimpleSet(new HashSet<>());
        orig.getSimpleSet().add(testA);
        testAClone.setStringAttribute(testA.getStringAttribute());
        clone.getSimpleSet().add(testAClone);
        final URI aUri = URI.create("http://krizik.felk.cvut.cz/ontologies#testAA");
        final OWLClassA extraA = new OWLClassA(aUri);
        extraA.setStringAttribute("string");
        orig.getSimpleSet().add(extraA);
        final OWLClassA extraAClone = new OWLClassA(aUri);
        extraAClone.setStringAttribute(extraA.getStringAttribute());
        clone.getSimpleSet().add(extraAClone);
    }

    @Test
    public void twoSetsWithManagedElementsOneElementReplacedWithNewWithoutIdHaveChanges() throws Exception {
        final OWLClassF testF = new OWLClassF();
        final OWLClassF cloneF = new OWLClassF();
        initFAndClone(testF, cloneF);
        final OWLClassA added = new OWLClassA();
        added.setStringAttribute("differentString");
        // OWLClassA does not have generated id, but for purposes of this test, let's assume it does
        final Iterator<OWLClassA> remove = cloneF.getSimpleSet().iterator();
        remove.next();
        remove.remove();
        cloneF.getSimpleSet().add(added);
        initMetamodelForClassA();
        final boolean res = manager.hasChanges(testF, cloneF);
        assertTrue(res);
    }

    @Test
    public void changeToTransientFieldIsIgnored() throws Exception {
        final OWLClassO testO = new OWLClassO();
        testO.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies#testO"));
        testO.setStringAttribute("String");
        final OWLClassO testOClone = new OWLClassO();
        testOClone.setUri(testO.getUri());
        testOClone.setStringAttribute(testO.getStringAttribute());
        testOClone.setTransientField("Change!");

        final boolean res = manager.hasChanges(testO, testOClone);
        assertFalse(res);
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

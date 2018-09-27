/**
 * Copyright (C) 2016 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.adapters.IndirectCollection;
import cz.cvut.kbss.jopa.environment.*;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.environment.utils.TestEnvironmentUtils;
import cz.cvut.kbss.jopa.model.MetamodelImpl;
import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.*;
import cz.cvut.kbss.jopa.sessions.change.ChangeRecordImpl;
import cz.cvut.kbss.jopa.sessions.change.ChangeSetFactory;
import cz.cvut.kbss.jopa.utils.CollectionFactory;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.lang.reflect.Field;
import java.net.URI;
import java.time.LocalDateTime;
import java.util.*;
import java.util.Map.Entry;

import static org.junit.Assert.*;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.*;

public class CloneBuilderTest {

    private CloneBuilderImpl builder;

    private OWLClassA entityA;
    private OWLClassB entityB;
    private OWLClassC entityC;
    private OWLClassD entityD;
    private OWLClassM entityM;
    private OWLClassQ entityQ;
    private static Set<Class<?>> managedTypes;
    private static EntityDescriptor defaultDescriptor;

    @Mock
    private UnitOfWorkImpl uow;

    @Mock
    private MetamodelImpl metamodel;

    private MetamodelMocks metamodelMocks;

    @BeforeClass
    public static void setUpBeforeClass() {
        initManagedTypes();
        defaultDescriptor = new EntityDescriptor();
    }

    private static void initManagedTypes() {
        managedTypes = new HashSet<>();
        managedTypes.add(OWLClassA.class);
        managedTypes.add(OWLClassB.class);
        managedTypes.add(OWLClassC.class);
        managedTypes.add(OWLClassD.class);
        managedTypes.add(OWLClassG.class);
        managedTypes.add(OWLClassH.class);
        managedTypes.add(OWLClassM.class);
        managedTypes.add(OWLClassO.class);
        managedTypes.add(OWLClassQ.class);
    }

    @Before
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        when(uow.isEntityType(any())).thenAnswer(invocation -> {
            Class<?> cls = (Class<?>) invocation.getArguments()[0];
            return managedTypes.contains(cls);
        });
        when(uow.getMetamodel()).thenReturn(metamodel);
        when(uow.registerExistingObject(any(), any(), any())).thenAnswer(
                invocation -> {
                    Object obj = invocation.getArguments()[0];
                    Descriptor desc = (Descriptor) invocation.getArguments()[1];
                    return builder.buildClone(obj, new CloneConfiguration(desc));
                });
        this.metamodelMocks = new MetamodelMocks();
        metamodelMocks.setMocks(metamodel);
        this.builder = new CloneBuilderImpl(uow);
        initValues();
        final CollectionFactory cf = new CollectionFactory(uow);
        when(uow.createIndirectCollection(any(), any(), any(Field.class))).thenAnswer(call -> {
            final Object[] args = call.getArguments();
            return cf.createIndirectCollection(args[0], args[1], (Field) args[2]);
        });
    }

    private void initValues() {
        entityA = new OWLClassA();
        final URI pk = URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityA");
        entityA.setUri(pk);
        entityA.setStringAttribute("TEST");
        Set<String> types = new HashSet<>();
        types.add("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityA");
        types.add("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityU");
        types.add("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityX");
        entityA.setTypes(types);
        OWLClassA t2 = new OWLClassA();
        final URI pk2 = URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityAA");
        t2.setUri(pk2);
        t2.setStringAttribute("TEST2");
        entityB = new OWLClassB();
        entityB.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityB"));
        entityB.setStringAttribute("someString");
        entityC = new OWLClassC();
        entityC.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityC"));
        entityD = new OWLClassD();
        entityD.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityD"));
        entityD.setOwlClassA(entityA);
        entityM = new OWLClassM();
        entityM.initializeTestValues(true);
        this.entityQ = new OWLClassQ();
        entityQ.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityC"));
        entityQ.setStringAttribute("stringAtt");
        entityQ.setParentString("parentStringAtt");
        entityQ.setLabel("OWLClassQ - instance");
        entityQ.setOwlClassA(entityA);
    }

    @Test
    public void testBuildClone() {
        OWLClassA res = (OWLClassA) builder.buildClone(entityA, new CloneConfiguration(defaultDescriptor));
        assertEquals(res.getStringAttribute(), entityA.getStringAttribute());
        assertEquals(res.getUri(), entityA.getUri());
        assertEquals(entityA.getTypes(), res.getTypes());
    }

    @Test(expected = NullPointerException.class)
    public void testBuildCloneNullOriginal() {
        builder.buildClone(null, new CloneConfiguration(defaultDescriptor));
        fail("This line should not have been reached.");
    }

    @Test(expected = NullPointerException.class)
    public void testBuildCloneNullContextUri() {
        builder.buildClone(entityA, null);
    }

    @Test(expected = NullPointerException.class)
    public void testBuildCloneNullCloneOwner() throws Exception {
        builder.buildClone(null, OWLClassB.getPropertiesField(), entityB, defaultDescriptor);
    }

    @Test
    public void testCloneCollection() {
        final OWLClassA clone = (OWLClassA) builder.buildClone(entityA, new CloneConfiguration(defaultDescriptor));
        assertEquals(entityA.getTypes().size(), clone.getTypes().size());
        for (String t : entityA.getTypes()) {
            assertTrue(clone.getTypes().contains(t));
        }
    }

    @Test
    public void testBuildCloneTwice() {
        OWLClassA res = (OWLClassA) this.builder.buildClone(entityA, new CloneConfiguration(defaultDescriptor));
        assertEquals(res.getStringAttribute(), entityA.getStringAttribute());
        assertEquals(res.getUri(), entityA.getUri());
        assertEquals(entityA.getTypes(), res.getTypes());
        assertNotSame(entityA, res);
        final OWLClassA resTwo = (OWLClassA) builder.buildClone(entityA, new CloneConfiguration(defaultDescriptor));
        assertSame(res, resTwo);
    }

    @Test
    public void testBuildCloneOriginalInUoW() {
        when(uow.containsOriginal(entityA)).thenReturn(Boolean.TRUE);
        when(uow.getCloneForOriginal(entityA)).thenReturn(entityA);
        final OWLClassA res = (OWLClassA) builder.buildClone(entityA, new CloneConfiguration(defaultDescriptor));
        assertNotNull(res);
        assertSame(entityA, res);
    }

    @Test
    public void testCloneListCollection() {
        final List<String> testList = new ArrayList<>();
        testList.add("One");
        testList.add("Two");
        testList.add("Three");
        @SuppressWarnings("unchecked")
        List<String> clone = (List<String>) builder.buildClone(testList, new CloneConfiguration(defaultDescriptor));
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
        final OWLClassA result = (OWLClassA) builder.buildClone(obj, new CloneConfiguration(defaultDescriptor));
        assertEquals(1, result.getTypes().size());
        assertEquals(type, result.getTypes().iterator().next());
    }

    @Test
    public void testCloneEmptyList() {
        entityC.setSimpleList(Collections.emptyList());
        entityC.setReferencedList(Generators.generateInstances(10));
        final OWLClassC res = (OWLClassC) builder.buildClone(entityC, new CloneConfiguration(defaultDescriptor));
        assertNotNull(res);
        assertTrue(res.getSimpleList().isEmpty());
        for (int i = 0; i < res.getReferencedList().size(); i++) {
            assertEquals(entityC.getReferencedList().get(i).getUri(), res.getReferencedList().get(i).getUri());
        }
    }

    @Test
    public void testCloneEmptySet() {
        final OWLClassA obj = new OWLClassA();
        final URI pk = URI.create("http://singletonTest");
        obj.setUri(pk);
        obj.setStringAttribute("TEST");
        obj.setTypes(Collections.emptySet());
        final OWLClassA res = (OWLClassA) builder.buildClone(obj, new CloneConfiguration(defaultDescriptor));
        assertNotNull(res);
        assertTrue(res.getTypes().isEmpty());
    }

    @Test
    public void testCloneProperties() {
        entityB.setProperties(Generators.generateStringProperties());
        OWLClassB res = (OWLClassB) builder.buildClone(entityB, new CloneConfiguration(defaultDescriptor));
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
    public void testCloneEmptyMap() {
        entityB.setProperties(Collections.emptyMap());
        OWLClassB res = (OWLClassB) builder.buildClone(entityB, new CloneConfiguration(defaultDescriptor));
        assertNotNull(res);
        assertTrue(res.getProperties().isEmpty());
    }

    @Test
    public void testCloneObjectProperty() {
        final OWLClassD another = new OWLClassD();
        another.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityDD"));
        another.setOwlClassA(entityA);
        final OWLClassD clOne = (OWLClassD) builder.buildClone(entityD, new CloneConfiguration(defaultDescriptor));
        assertNotSame(entityD, clOne);
        assertNotSame(entityA, clOne.getOwlClassA());
        final OWLClassD clTwo = (OWLClassD) builder.buildClone(another, new CloneConfiguration(defaultDescriptor));
        assertSame(clOne.getOwlClassA(), clTwo.getOwlClassA());
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
        final OWLClassB res = (OWLClassB) builder.buildClone(entityB, new CloneConfiguration(defaultDescriptor));
        assertNotSame(entityB, res);
        assertNull(res.getProperties());
    }

    @Test
    public void testCloneSingletonSet() {
        final Set<String> singleton = Collections
                .singleton("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityY");
        entityA.setTypes(singleton);
        final OWLClassA res = (OWLClassA) builder.buildClone(entityA, new CloneConfiguration(defaultDescriptor));
        assertNotSame(entityA, res);
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
        final OWLClassB res = (OWLClassB) builder.buildClone(entityB, new CloneConfiguration(defaultDescriptor));
        assertNotNull(res);
        assertNotSame(entityB, res);
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
        final OWLClassC res = (OWLClassC) builder.buildClone(entityC, new CloneConfiguration(defaultDescriptor));
        assertNotSame(res, entityC);
        assertEquals(1, res.getReferencedList().size());
        assertTrue(res.getReferencedList() instanceof IndirectCollection);
        final OWLClassA a = res.getReferencedList().get(0);
        assertNotSame(entityA, a);
        assertEquals(entityA.getUri(), a.getUri());
        assertTrue(a.getTypes() instanceof IndirectCollection);
    }

    @Test
    public void testCloneReferencedList() {
        // Let's see how long this takes
        entityC.setReferencedList(Generators.generateInstances(100));
        final OWLClassC res = (OWLClassC) builder.buildClone(entityC, new CloneConfiguration(defaultDescriptor));
        assertNotSame(entityC, res);
        int size = entityC.getReferencedList().size();
        assertEquals(size, res.getReferencedList().size());
        assertTrue(res.getReferencedList() instanceof IndirectCollection);
        for (int i = 0; i < size; i++) {
            final OWLClassA or = entityC.getReferencedList().get(i);
            final OWLClassA cl = res.getReferencedList().get(i);
            assertNotSame(or, cl);
            assertEquals(or.getUri(), cl.getUri());
            assertEquals(or.getStringAttribute(), cl.getStringAttribute());
            assertEquals(or.getTypes().size(), cl.getTypes().size());
            assertTrue(cl.getTypes() instanceof IndirectCollection);
        }
    }

    @Test
    public void testCloneReferencedListWithNulls() {
        final List<OWLClassA> nulls = new ArrayList<>();
        for (int i = 0; i < 5; i++) {
            nulls.add(null);
        }
        entityC.setReferencedList(nulls);
        final OWLClassC res = (OWLClassC) builder.buildClone(entityC, new CloneConfiguration(defaultDescriptor));
        assertNotSame(entityC, res);
        int size = entityC.getReferencedList().size();
        assertEquals(size, res.getReferencedList().size());
        assertTrue(res.getReferencedList() instanceof IndirectCollection);
        for (OWLClassA a : res.getReferencedList()) {
            assertNull(a);
        }
    }

    @Test
    public void testMergeChangesOnString() {
        final OWLClassA a = new OWLClassA();
        a.setUri(entityA.getUri());
        a.setStringAttribute("oldString");
        final OWLClassA cloneA = (OWLClassA) builder.buildClone(a, new CloneConfiguration(defaultDescriptor));
        final String newStrAtt = "newString";
        cloneA.setStringAttribute(newStrAtt);
        final ObjectChangeSet chSet = ChangeSetFactory.createObjectChangeSet(a, cloneA,
                defaultDescriptor);
        chSet.addChangeRecord(new ChangeRecordImpl(metamodelMocks.forOwlClassA().stringAttribute(),
                newStrAtt));
        builder.mergeChanges(chSet);

        assertEquals(newStrAtt, a.getStringAttribute());
    }

    @Test
    public void testMergeChangesPropertiesFromNull() {
        final OWLClassB b = (OWLClassB) builder.buildClone(entityB, new CloneConfiguration(defaultDescriptor));
        assertNull(b.getProperties());
        b.setProperties(Generators.generateStringProperties());
        final ObjectChangeSet chSet = ChangeSetFactory.createObjectChangeSet(entityB, b,
                defaultDescriptor);
        chSet.addChangeRecord(new ChangeRecordImpl(metamodelMocks.forOwlClassB().propertiesSpec(),
                b.getProperties()));
        builder.mergeChanges(chSet);

        assertNotNull(entityB.getProperties());
        assertEquals(b.getProperties(), entityB.getProperties());
    }

    @Test
    public void testMergeChangesRefListFromNull() {
        final OWLClassC c = (OWLClassC) builder.buildClone(entityC, new CloneConfiguration(defaultDescriptor));
        assertNotSame(entityC, c);
        assertNull(entityC.getReferencedList());
        c.setReferencedList(Generators.generateInstances(5));
        final ObjectChangeSet chSet = ChangeSetFactory.createObjectChangeSet(entityC, c,
                defaultDescriptor);
        chSet.addChangeRecord(new ChangeRecordImpl(metamodelMocks.forOwlClassC().referencedListAtt(),
                c.getReferencedList()));
        builder.mergeChanges(chSet);

        assertNotNull(entityC.getReferencedList());
        for (int i = 0; i < c.getReferencedList().size(); i++) {
            assertEquals(c.getReferencedList().get(i).getUri(), entityC.getReferencedList().get(i)
                                                                       .getUri());
        }
    }

    @Test
    public void mergeChangesHandlesSingletonCollection() {
        entityC.setReferencedList(Collections.singletonList(entityA));
        final OWLClassC c = (OWLClassC) builder.buildClone(entityC, new CloneConfiguration(defaultDescriptor));
        final OWLClassA newA = new OWLClassA();
        c.setReferencedList(Collections.singletonList(newA));
        final ObjectChangeSet chSet = ChangeSetFactory.createObjectChangeSet(entityC, c,
                defaultDescriptor);
        chSet.addChangeRecord(
                new ChangeRecordImpl(metamodelMocks.forOwlClassC().referencedListAtt(), c.getReferencedList()));

        builder.mergeChanges(chSet);
        assertNotNull(entityC.getReferencedList());
        assertEquals(1, entityC.getReferencedList().size());
        assertEquals(newA, entityC.getReferencedList().get(0));
    }

    @Test
    public void testBuildCloneWithMultipleWrapperTypesAndStringKey() {
        final OWLClassM m = (OWLClassM) builder.buildClone(entityM, new CloneConfiguration(defaultDescriptor));
        assertNotSame(entityM, m);
        assertEquals(entityM.getKey(), m.getKey());
        assertEquals(entityM.getBooleanAttribute(), m.getBooleanAttribute());
        assertEquals(entityM.getIntAttribute(), m.getIntAttribute());
        assertEquals(entityM.getLongAttribute(), m.getLongAttribute());
        assertEquals(entityM.getDoubleAttribute(), m.getDoubleAttribute());
        assertEquals(entityM.getDateAttribute(), m.getDateAttribute());
    }

    @Test
    public void testMergeChangesWithMultipleWrapperTypesAndStringKey() {
        final OWLClassM m = (OWLClassM) builder.buildClone(entityM, new CloneConfiguration(defaultDescriptor));
        assertNotSame(entityM, m);
        final ObjectChangeSet changeSet = ChangeSetFactory.createObjectChangeSet(entityM, m, defaultDescriptor);
        m.setBooleanAttribute(!m.getBooleanAttribute());
        changeSet.addChangeRecord(
                new ChangeRecordImpl(metamodelMocks.forOwlClassM().booleanAttribute(), m.getBooleanAttribute()));
        m.setIntAttribute(11111);
        changeSet
                .addChangeRecord(
                        new ChangeRecordImpl(metamodelMocks.forOwlClassM().integerAttribute(), m.getIntAttribute()));
        m.setLongAttribute(999L);
        changeSet.addChangeRecord(
                new ChangeRecordImpl(metamodelMocks.forOwlClassM().longAttribute(), m.getLongAttribute()));
        m.setDoubleAttribute(1.1);
        changeSet.addChangeRecord(
                new ChangeRecordImpl(metamodelMocks.forOwlClassM().doubleAttribute(), m.getDoubleAttribute()));
        m.setDateAttribute(new Date(System.currentTimeMillis() + 10000L));
        changeSet.addChangeRecord(
                new ChangeRecordImpl(metamodelMocks.forOwlClassM().dateAttribute(), m.getDateAttribute()));

        builder.mergeChanges(changeSet);
        assertEquals(m.getBooleanAttribute(), entityM.getBooleanAttribute());
        assertEquals(m.getIntAttribute(), entityM.getIntAttribute());
        assertEquals(m.getLongAttribute(), entityM.getLongAttribute());
        assertEquals(m.getDoubleAttribute(), entityM.getDoubleAttribute());
        assertEquals(m.getDateAttribute(), entityM.getDateAttribute());
    }

    @Test
    public void cloningSkipsTransientFields() {
        final OWLClassO entityO = new OWLClassO();
        entityO.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies#entityO"));
        entityO.setStringAttribute("String");
        entityO.setTransientField("Transient");
        entityO.setTransientFieldWithAnnotation("TransientAnnotated");

        final OWLClassO clone = (OWLClassO) builder.buildClone(entityO, new CloneConfiguration(defaultDescriptor));
        assertNotNull(clone);
        assertEquals(entityO.getUri(), clone.getUri());
        assertEquals(entityO.getStringAttribute(), clone.getStringAttribute());
        assertNull(clone.getTransientField());
        assertNull(clone.getTransientFieldWithAnnotation());
    }

    @Test
    public void reusesAlreadyClonedInstancesWhenCloningCollections() {
        final OWLClassA cloneA = (OWLClassA) builder.buildClone(entityA, new CloneConfiguration(defaultDescriptor));
        entityC.setReferencedList(new ArrayList<>());
        entityC.getReferencedList().add(entityA);

        final OWLClassC cloneC = (OWLClassC) builder.buildClone(entityC, new CloneConfiguration(defaultDescriptor));
        assertNotNull(cloneC.getReferencedList());
        assertEquals(1, cloneC.getReferencedList().size());
        assertSame(cloneA, cloneC.getReferencedList().get(0));
    }

    @Test
    public void cloneBuildingHandlesCyclesInObjectGraphByRegisteringAlreadyVisitedObjects() {
        final OWLClassG entityG = initGWithBackwardReference();

        final OWLClassG cloneG = (OWLClassG) builder.buildClone(entityG, new CloneConfiguration(defaultDescriptor));
        assertNotNull(cloneG);
        assertNotNull(cloneG.getOwlClassH());
        assertSame(cloneG, cloneG.getOwlClassH().getOwlClassG());
    }

    private OWLClassG initGWithBackwardReference() {
        final OWLClassG g = new OWLClassG(URI.create("http://krizik.felk.cvut.cz/ontologies#entityG"));
        final OWLClassH h = new OWLClassH(URI.create("http://krizik.felk.cvut.cz/ontologies#entityH"));
        g.setOwlClassH(h);
        h.setOwlClassG(g);
        return g;
    }

    @Test
    public void testBuildCloneOfInstanceWithArrayAsListFieldValue() {
        final OWLClassC instance = new OWLClassC(URI.create("http://test"));
        final OWLClassA another = new OWLClassA(URI.create("http://anotherA"));
        instance.setSimpleList(Arrays.asList(entityA, another));

        final OWLClassC result = (OWLClassC) builder.buildClone(instance, new CloneConfiguration(defaultDescriptor));
        assertNotNull(result);
        for (int i = 0; i < instance.getSimpleList().size(); i++) {
            assertEquals(instance.getSimpleList().get(i).getUri(), result.getSimpleList().get(i).getUri());
        }
    }

    @Test
    public void mergeOfFieldOfManagedTypeUsesOriginalValueForMerge() {
        entityD.setOwlClassA(entityA);
        final OWLClassD dClone = new OWLClassD();
        dClone.setUri(entityD.getUri());

        final OWLClassA newValue = new OWLClassA();
        newValue.setUri(URI.create(OWLClassA.getClassIri() + Generators.randomInt(1000)));
        newValue.setStringAttribute("SomeValue");
        final OWLClassA newValueClone = new OWLClassA();
        newValueClone.setUri(newValue.getUri());
        newValueClone.setStringAttribute(newValue.getStringAttribute());
        dClone.setOwlClassA(newValueClone);

        final ObjectChangeSet chSet = TestEnvironmentUtils.createObjectChangeSet(entityD, dClone, null);
        chSet.addChangeRecord(new ChangeRecordImpl(metamodelMocks.forOwlClassD().owlClassAAtt(), newValueClone));
        when(uow.getOriginal(newValueClone)).thenReturn(newValue);
        builder.mergeChanges(chSet);
        assertSame(newValue, entityD.getOwlClassA());
        assertEquals(newValue.getUri(), entityD.getOwlClassA().getUri());
        assertEquals(newValue.getStringAttribute(), entityD.getOwlClassA().getStringAttribute());
    }

    @Test
    public void buildCloneClonesMappedSuperclassFieldsAsWell() {

        final OWLClassQ clone = (OWLClassQ) builder.buildClone(entityQ, new CloneConfiguration(defaultDescriptor));
        assertNotNull(clone);
        assertEquals(entityQ.getUri(), clone.getUri());
        assertEquals(entityQ.getStringAttribute(), clone.getStringAttribute());
        assertEquals(entityQ.getParentString(), clone.getParentString());
        assertEquals(entityQ.getLabel(), clone.getLabel());
        assertNotNull(clone.getOwlClassA());
        assertEquals(entityQ.getOwlClassA().getUri(), clone.getOwlClassA().getUri());
        assertNotSame(entityQ.getOwlClassA(), clone.getOwlClassA());
    }

    @Test
    public void mergeChangesMergesChangesOnMappedSuperclassFields() {
        final OWLClassQ qClone = new OWLClassQ();
        qClone.setUri(entityQ.getUri());
        qClone.setStringAttribute("newStringAtt");
        qClone.setParentString("anotherStringAtt");
        final OWLClassA newA = new OWLClassA();
        newA.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/newA"));
        qClone.setOwlClassA(newA);
        final ObjectChangeSet changeSet = TestEnvironmentUtils.createObjectChangeSet(entityQ, qClone, null);
        changeSet.addChangeRecord(
                new ChangeRecordImpl(metamodelMocks.forOwlClassQ().qStringAtt(), qClone.getStringAttribute()));
        changeSet.addChangeRecord(
                new ChangeRecordImpl(metamodelMocks.forOwlClassQ().qParentStringAtt(), qClone.getParentString()));
        changeSet.addChangeRecord(
                new ChangeRecordImpl(metamodelMocks.forOwlClassQ().qOwlClassAAtt(), qClone.getOwlClassA()));

        builder.mergeChanges(changeSet);
        assertEquals(qClone.getStringAttribute(), entityQ.getStringAttribute());
        assertEquals(qClone.getParentString(), entityQ.getParentString());
        assertEquals(qClone.getOwlClassA().getUri(), entityQ.getOwlClassA().getUri());
    }

    @Test
    public void buildCloneDoesNotAttemptToRegisterCloneWithoutIdentifier() throws Exception {
        final A a = new A();
        final B b = new B();
        a.uri = Generators.createIndividualIdentifier();
        b.uri = Generators.createIndividualIdentifier();
        a.b = Collections.singleton(b);
        b.a = a;
        initMetamodelForAB();
        doAnswer(invocation -> {
            Object obj = invocation.getArguments()[0];
            Descriptor desc = (Descriptor) invocation.getArguments()[1];
            final Object clone = builder.buildClone(obj, new CloneConfiguration(desc));
            // THIS is the important verification
            assertNotNull(EntityPropertiesUtils.getIdentifier(clone, metamodel));
            return clone;
        }).when(uow).registerExistingObject(any(), any(), any());

        final B result = (B) builder.buildClone(b, new CloneConfiguration(defaultDescriptor));
        assertNotNull(result);
        assertNotNull(result.a);
        assertEquals(result, result.a.b.iterator().next());
        assertEquals(b.uri, result.uri);
        assertEquals(a.uri, result.a.uri);
    }

    private void initMetamodelForAB() throws Exception {
        when(uow.isEntityType(A.class)).thenReturn(true);
        when(uow.isEntityType(B.class)).thenReturn(true);
        final EntityTypeImpl<A> etA = mock(EntityTypeImpl.class);
        final EntityTypeImpl<B> etB = mock(EntityTypeImpl.class);
        final Identifier idA = new IRIIdentifierImpl<>(etA, A.class.getDeclaredField("uri"), true);
        final Identifier idB = new IRIIdentifierImpl<>(etB, B.class.getDeclaredField("uri"), true);
        when(etA.getIdentifier()).thenReturn(idA);
        when(etB.getIdentifier()).thenReturn(idB);
        final PluralAttribute hasB = mock(PluralAttribute.class);
        when(hasB.getJavaField()).thenReturn(A.class.getDeclaredField("b"));
        when(hasB.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.OBJECT);
        when(hasB.getCollectionType()).thenReturn(PluralAttribute.CollectionType.SET);
        when(hasB.getElementType()).thenReturn(etB);
        final Set aFieldSpecs = new HashSet<>(2);
        aFieldSpecs.add(idA);
        aFieldSpecs.add(hasB);
        when(etA.getFieldSpecifications()).thenReturn(aFieldSpecs);
        when(etA.getFieldSpecification("b")).thenReturn(hasB);
        final SingularAttribute hasA = mock(SingularAttribute.class);
        when(hasA.getJavaField()).thenReturn(B.class.getDeclaredField("a"));
        when(hasA.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.OBJECT);
        final Set bFieldSpecs = new HashSet<>(2);
        bFieldSpecs.add(idB);
        bFieldSpecs.add(hasA);
        when(etB.getFieldSpecifications()).thenReturn(bFieldSpecs);
        when(etB.getFieldSpecification("a")).thenReturn(hasA);
        doReturn(etA).when(metamodel).entity(A.class);
        doReturn(etB).when(metamodel).entity(B.class);
    }

    private static class A {
        @Id
        private URI uri;

        @OWLObjectProperty(iri = Vocabulary.ATTRIBUTE_BASE + "hasB")
        private Set<B> b;
    }

    private static class B {
        @Id
        private URI uri;

        @OWLObjectProperty(iri = Vocabulary.ATTRIBUTE_BASE + "hasA")
        private A a;
    }

    @Test
    public void buildCloneCopiesReturnsSameInstanceForImmutableClasses() {
        final LocalDateTime instance = LocalDateTime.now();
        assertSame(instance, builder.buildClone(instance, new CloneConfiguration(defaultDescriptor)));
    }
}

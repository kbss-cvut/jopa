/*
 * JOPA
 * Copyright (C) 2025 Czech Technical University in Prague
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */
package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassB;
import cz.cvut.kbss.jopa.environment.OWLClassC;
import cz.cvut.kbss.jopa.environment.OWLClassD;
import cz.cvut.kbss.jopa.environment.OWLClassG;
import cz.cvut.kbss.jopa.environment.OWLClassH;
import cz.cvut.kbss.jopa.environment.OWLClassM;
import cz.cvut.kbss.jopa.environment.OWLClassO;
import cz.cvut.kbss.jopa.environment.OWLClassQ;
import cz.cvut.kbss.jopa.environment.Vocabulary;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.model.LoadState;
import cz.cvut.kbss.jopa.model.MetamodelImpl;
import cz.cvut.kbss.jopa.model.annotations.FetchType;
import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.CollectionType;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.IRIIdentifierImpl;
import cz.cvut.kbss.jopa.model.metamodel.IdentifiableEntityType;
import cz.cvut.kbss.jopa.model.metamodel.Identifier;
import cz.cvut.kbss.jopa.model.metamodel.PluralAttribute;
import cz.cvut.kbss.jopa.model.metamodel.SingularAttribute;
import cz.cvut.kbss.jopa.proxy.change.ChangeTrackingIndirectCollection;
import cz.cvut.kbss.jopa.proxy.lazy.LazyLoadingProxy;
import cz.cvut.kbss.jopa.sessions.change.ChangeRecord;
import cz.cvut.kbss.jopa.sessions.change.ChangeSetFactory;
import cz.cvut.kbss.jopa.sessions.change.ObjectChangeSet;
import cz.cvut.kbss.jopa.sessions.descriptor.LoadStateDescriptor;
import cz.cvut.kbss.jopa.sessions.descriptor.LoadStateDescriptorFactory;
import cz.cvut.kbss.jopa.sessions.util.CloneConfiguration;
import cz.cvut.kbss.jopa.sessions.util.CloneRegistrationDescriptor;
import cz.cvut.kbss.jopa.sessions.util.LoadStateDescriptorRegistry;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.lang.reflect.Field;
import java.net.URI;
import java.time.Instant;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class CloneBuilderTest {

    private CloneBuilder builder;

    private OWLClassA entityA;
    private OWLClassB entityB;
    private OWLClassC entityC;
    private OWLClassD entityD;
    private OWLClassM entityM;
    private OWLClassQ entityQ;
    private static Set<Class<?>> managedTypes;
    private static EntityDescriptor defaultDescriptor;

    @Mock
    private AbstractUnitOfWork uow;

    private final LoadStateDescriptorRegistry loadStateRegistry = new LoadStateDescriptorRegistry(Object::toString);

    @Mock
    private MetamodelImpl metamodel;

    private MetamodelMocks metamodelMocks;

    @BeforeAll
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

    @BeforeEach
    public void setUp() throws Exception {
        when(uow.isEntityType(any())).thenAnswer(invocation -> {
            Class<?> cls = (Class<?>) invocation.getArguments()[0];
            return managedTypes.contains(cls);
        });
        when(uow.getMetamodel()).thenReturn(metamodel);
        when(uow.registerExistingObject(any(), any(CloneRegistrationDescriptor.class))).thenAnswer(
                invocation -> {
                    Object obj = invocation.getArguments()[0];
                    CloneRegistrationDescriptor desc = (CloneRegistrationDescriptor) invocation.getArguments()[1];
                    return builder.buildClone(obj, new CloneConfiguration(desc.getDescriptor(), true));
                });
        this.metamodelMocks = new MetamodelMocks();
        metamodelMocks.setMocks(metamodel);
        this.builder = new CloneBuilder(uow);
        initValues();
        final IndirectWrapperHelper cf = new IndirectWrapperHelper(uow);
        when(uow.createIndirectCollection(any(), any(), any())).thenAnswer(call -> {
            final Object[] args = call.getArguments();
            return cf.createIndirectWrapper(args[0], args[1], (Field) args[2]);
        });
        when(uow.getLoadStateRegistry()).thenReturn(loadStateRegistry);
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
        defaultLoadStateDescriptor(entityA, metamodelMocks.forOwlClassA().entityType());
        OWLClassA res = (OWLClassA) builder.buildClone(entityA, new CloneConfiguration(defaultDescriptor, false));
        assertEquals(res.getStringAttribute(), entityA.getStringAttribute());
        assertEquals(res.getUri(), entityA.getUri());
        assertEquals(entityA.getTypes(), res.getTypes());
    }

    private <T> void defaultLoadStateDescriptor(T entity, EntityType<T> et) {
        final LoadStateDescriptor<T> desc = LoadStateDescriptorFactory.createAllLoaded(entity, et);
        loadStateRegistry.put(entity, desc);
    }

    @Test
    public void testBuildCloneNullOriginal() {
        assertThrows(NullPointerException.class,
                () -> builder.buildClone(null, new CloneConfiguration(defaultDescriptor, false)));
    }

    @Test
    public void testBuildCloneNullContextUri() {
        assertThrows(NullPointerException.class, () -> builder.buildClone(entityA, null));
    }

    @Test
    public void testBuildCloneNullCloneOwner() {
        assertThrows(NullPointerException.class,
                () -> builder.buildClone(null, OWLClassB.getPropertiesField(), entityB, defaultDescriptor));
    }

    @Test
    public void testCloneCollection() {
        defaultLoadStateDescriptor(entityA, metamodelMocks.forOwlClassA().entityType());
        final OWLClassA clone = (OWLClassA) builder.buildClone(entityA, new CloneConfiguration(defaultDescriptor, false));
        assertEquals(entityA.getTypes().size(), clone.getTypes().size());
        for (String t : entityA.getTypes()) {
            assertTrue(clone.getTypes().contains(t));
        }
    }

    @Test
    public void testBuildCloneTwice() {
        defaultLoadStateDescriptor(entityA, metamodelMocks.forOwlClassA().entityType());
        OWLClassA res = (OWLClassA) this.builder.buildClone(entityA, new CloneConfiguration(defaultDescriptor, false));
        assertEquals(res.getStringAttribute(), entityA.getStringAttribute());
        assertEquals(res.getUri(), entityA.getUri());
        assertEquals(entityA.getTypes(), res.getTypes());
        assertNotSame(entityA, res);
        final OWLClassA resTwo = (OWLClassA) builder.buildClone(entityA, new CloneConfiguration(defaultDescriptor, false));
        assertSame(res, resTwo);
    }

    @Test
    public void testBuildCloneOriginalInUoW() {
        defaultLoadStateDescriptor(entityA, metamodelMocks.forOwlClassA().entityType());
        when(uow.containsOriginal(entityA)).thenReturn(Boolean.TRUE);
        when(uow.getCloneForOriginal(entityA)).thenReturn(entityA);
        final OWLClassA res = (OWLClassA) builder.buildClone(entityA, new CloneConfiguration(defaultDescriptor, false));
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
        List<String> clone = (List<String>) builder.buildClone(testList, new CloneConfiguration(defaultDescriptor, false));
        assertEquals(testList.size(), clone.size());
        Iterator<String> it1 = testList.iterator();
        Iterator<String> it2 = clone.iterator();
        while (it1.hasNext() && it2.hasNext()) {
            assertEquals(it1.next(), it2.next());
        }
    }

    @Test
    public void testCloneSingletonCollection() {
        defaultLoadStateDescriptor(entityA, metamodelMocks.forOwlClassA().entityType());
        String type = "A_type";
        entityA.setTypes(Collections.singleton(type));
        final OWLClassA result = (OWLClassA) builder.buildClone(entityA, new CloneConfiguration(defaultDescriptor, false));
        assertEquals(1, result.getTypes().size());
        assertEquals(type, result.getTypes().iterator().next());
    }

    @Test
    public void testCloneEmptyList() {
        defaultLoadStateDescriptor(entityC, metamodelMocks.forOwlClassC().entityType());
        entityC.setSimpleList(Collections.emptyList());
        entityC.setReferencedList(Generators.generateInstances(10));
        entityC.getReferencedList().forEach(a -> defaultLoadStateDescriptor(a, metamodelMocks.forOwlClassA().entityType()));
        final OWLClassC res = (OWLClassC) builder.buildClone(entityC, new CloneConfiguration(defaultDescriptor, false));
        assertNotNull(res);
        assertTrue(res.getSimpleList().isEmpty());
        for (int i = 0; i < res.getReferencedList().size(); i++) {
            assertEquals(entityC.getReferencedList().get(i).getUri(), res.getReferencedList().get(i).getUri());
        }
    }

    @Test
    public void testCloneEmptySet() {
        defaultLoadStateDescriptor(entityA, metamodelMocks.forOwlClassA().entityType());
        entityA.setTypes(Collections.emptySet());
        final OWLClassA res = (OWLClassA) builder.buildClone(entityA, new CloneConfiguration(defaultDescriptor, false));
        assertNotNull(res);
        assertTrue(res.getTypes().isEmpty());
    }

    @Test
    public void testCloneProperties() {
        defaultLoadStateDescriptor(entityB, metamodelMocks.forOwlClassB().entityType());
        entityB.setProperties(Generators.generateStringProperties());
        OWLClassB res = (OWLClassB) builder.buildClone(entityB, new CloneConfiguration(defaultDescriptor, false));
        assertNotNull(res);
        assertEquals(entityB.getUri(), res.getUri());
        assertEquals(entityB.getStringAttribute(), res.getStringAttribute());
        assertEquals(entityB.getProperties().size(), res.getProperties().size());
        assertInstanceOf(ChangeTrackingIndirectCollection.class, res.getProperties());
        for (Entry<String, Set<String>> e : entityB.getProperties().entrySet()) {
            final String k = e.getKey();
            assertTrue(res.getProperties().containsKey(k));
            final Set<String> rv = res.getProperties().get(k);
            assertInstanceOf(ChangeTrackingIndirectCollection.class, rv);
            assertEquals(e.getValue().size(), rv.size());
            for (String s : e.getValue()) {
                assertTrue(rv.contains(s));
            }
        }
    }

    @Test
    public void testCloneEmptyMap() {
        defaultLoadStateDescriptor(entityB, metamodelMocks.forOwlClassB().entityType());
        entityB.setProperties(Collections.emptyMap());
        OWLClassB res = (OWLClassB) builder.buildClone(entityB, new CloneConfiguration(defaultDescriptor, false));
        assertNotNull(res);
        assertTrue(res.getProperties().isEmpty());
    }

    @Test
    public void testCloneObjectProperty() {
        defaultLoadStateDescriptor(entityD, metamodelMocks.forOwlClassD().entityType());
        defaultLoadStateDescriptor(entityA, metamodelMocks.forOwlClassA().entityType());
        final OWLClassD another = new OWLClassD();
        another.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityDD"));
        another.setOwlClassA(entityA);
        defaultLoadStateDescriptor(another, metamodelMocks.forOwlClassD().entityType());
        final OWLClassD clOne = (OWLClassD) builder.buildClone(entityD, new CloneConfiguration(defaultDescriptor, false));
        assertNotSame(entityD, clOne);
        assertNotSame(entityA, clOne.getOwlClassA());
        final OWLClassD clTwo = (OWLClassD) builder.buildClone(another, new CloneConfiguration(defaultDescriptor, false));
        assertSame(clOne.getOwlClassA(), clTwo.getOwlClassA());
        assertEquals(entityA.getStringAttribute(), clOne.getOwlClassA().getStringAttribute());
        assertInstanceOf(ChangeTrackingIndirectCollection.class, clOne.getOwlClassA().getTypes());
        final Set<String> tps = clOne.getOwlClassA().getTypes();
        assertEquals(entityA.getTypes().size(), tps.size());
        for (String t : entityA.getTypes()) {
            assertTrue(tps.contains(t));
        }
    }

    @Test
    public void testCloneWithNullCollection() {
        defaultLoadStateDescriptor(entityB, metamodelMocks.forOwlClassB().entityType());
        assertNull(entityB.getProperties());
        final OWLClassB res = (OWLClassB) builder.buildClone(entityB, new CloneConfiguration(defaultDescriptor, false));
        assertNotSame(entityB, res);
        assertNull(res.getProperties());
    }

    @Test
    public void testCloneSingletonMap() {
        defaultLoadStateDescriptor(entityB, metamodelMocks.forOwlClassB().entityType());
        final String key = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#attr";
        final String value = "stringValue";
        final Map<String, Set<String>> m = Collections.singletonMap(key,
                Collections.singleton(value));
        entityB.setProperties(m);
        final OWLClassB res = (OWLClassB) builder.buildClone(entityB, new CloneConfiguration(defaultDescriptor, false));
        assertNotNull(res);
        assertNotSame(entityB, res);
        assertEquals(1, res.getProperties().size());
        assertInstanceOf(ChangeTrackingIndirectCollection.class, res.getProperties());
        final Set<String> s = res.getProperties().get(key);
        assertInstanceOf(ChangeTrackingIndirectCollection.class, s);
        assertEquals(1, s.size());
        assertEquals(value, s.iterator().next());
    }

    @Test
    public void testCloneSingletonListWithReference() {
        defaultLoadStateDescriptor(entityC, metamodelMocks.forOwlClassC().entityType());
        defaultLoadStateDescriptor(entityA, metamodelMocks.forOwlClassA().entityType());
        entityC.setReferencedList(Collections.singletonList(entityA));
        final OWLClassC res = (OWLClassC) builder.buildClone(entityC, new CloneConfiguration(defaultDescriptor, false));
        assertNotSame(res, entityC);
        assertEquals(1, res.getReferencedList().size());
        assertInstanceOf(ChangeTrackingIndirectCollection.class, res.getReferencedList());
        final OWLClassA a = res.getReferencedList().get(0);
        assertNotSame(entityA, a);
        assertEquals(entityA.getUri(), a.getUri());
        assertInstanceOf(ChangeTrackingIndirectCollection.class, a.getTypes());
    }

    @Test
    public void testCloneReferencedList() {
        defaultLoadStateDescriptor(entityC, metamodelMocks.forOwlClassC().entityType());
        entityC.setReferencedList(Generators.generateInstances(10));
        entityC.getReferencedList().forEach(a -> defaultLoadStateDescriptor(a, metamodelMocks.forOwlClassA().entityType()));
        final OWLClassC res = (OWLClassC) builder.buildClone(entityC, new CloneConfiguration(defaultDescriptor, false));
        assertNotSame(entityC, res);
        int size = entityC.getReferencedList().size();
        assertEquals(size, res.getReferencedList().size());
        assertInstanceOf(ChangeTrackingIndirectCollection.class, res.getReferencedList());
        for (int i = 0; i < size; i++) {
            final OWLClassA or = entityC.getReferencedList().get(i);
            final OWLClassA cl = res.getReferencedList().get(i);
            assertNotSame(or, cl);
            assertEquals(or.getUri(), cl.getUri());
            assertEquals(or.getStringAttribute(), cl.getStringAttribute());
            assertEquals(or.getTypes().size(), cl.getTypes().size());
            assertInstanceOf(ChangeTrackingIndirectCollection.class, cl.getTypes());
        }
    }

    @Test
    public void testCloneReferencedListWithNulls() {
        defaultLoadStateDescriptor(entityC, metamodelMocks.forOwlClassC().entityType());
        final List<OWLClassA> nulls = new ArrayList<>(Arrays.asList(null, null, null, null));
        entityC.setReferencedList(nulls);
        final OWLClassC res = (OWLClassC) builder.buildClone(entityC, new CloneConfiguration(defaultDescriptor, false));
        assertNotSame(entityC, res);
        int size = entityC.getReferencedList().size();
        assertEquals(size, res.getReferencedList().size());
        assertInstanceOf(ChangeTrackingIndirectCollection.class, res.getReferencedList());
        for (OWLClassA a : res.getReferencedList()) {
            assertNull(a);
        }
    }

    @Test
    public void testMergeChangesOnString() {
        final OWLClassA a = new OWLClassA(entityA.getUri());
        a.setStringAttribute("oldString");
        defaultLoadStateDescriptor(a, metamodelMocks.forOwlClassA().entityType());
        final OWLClassA cloneA = (OWLClassA) builder.buildClone(a, new CloneConfiguration(defaultDescriptor, false));
        final String newStrAtt = "newString";
        cloneA.setStringAttribute(newStrAtt);
        final ObjectChangeSet chSet = ChangeSetFactory.createObjectChangeSet(a, cloneA, defaultDescriptor);
        chSet.addChangeRecord(new ChangeRecord(metamodelMocks.forOwlClassA().stringAttribute(),
                newStrAtt));
        builder.mergeChanges(chSet);

        assertEquals(newStrAtt, a.getStringAttribute());
    }

    @Test
    public void testMergeChangesPropertiesFromNull() {
        defaultLoadStateDescriptor(entityB, metamodelMocks.forOwlClassB().entityType());
        final OWLClassB b = (OWLClassB) builder.buildClone(entityB, new CloneConfiguration(defaultDescriptor, false));
        assertNull(b.getProperties());
        b.setProperties(Generators.generateStringProperties());
        final ObjectChangeSet chSet = ChangeSetFactory.createObjectChangeSet(entityB, b,
                defaultDescriptor);
        chSet.addChangeRecord(new ChangeRecord(metamodelMocks.forOwlClassB().propertiesSpec(),
                b.getProperties()));
        builder.mergeChanges(chSet);

        assertNotNull(entityB.getProperties());
        assertEquals(b.getProperties(), entityB.getProperties());
    }

    @Test
    public void testMergeChangesRefListFromNull() {
        defaultLoadStateDescriptor(entityC, metamodelMocks.forOwlClassC().entityType());
        final OWLClassC c = (OWLClassC) builder.buildClone(entityC, new CloneConfiguration(defaultDescriptor, false));
        assertNotSame(entityC, c);
        assertNull(entityC.getReferencedList());
        c.setReferencedList(Generators.generateInstances(5));
        final ObjectChangeSet chSet = ChangeSetFactory.createObjectChangeSet(entityC, c, defaultDescriptor);
        chSet.addChangeRecord(new ChangeRecord(metamodelMocks.forOwlClassC().referencedListAtt(),
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
        defaultLoadStateDescriptor(entityC, metamodelMocks.forOwlClassC().entityType());
        defaultLoadStateDescriptor(entityA, metamodelMocks.forOwlClassA().entityType());
        entityC.setReferencedList(Collections.singletonList(entityA));
        final OWLClassC c = (OWLClassC) builder.buildClone(entityC, new CloneConfiguration(defaultDescriptor, false));
        final OWLClassA newA = new OWLClassA();
        c.setReferencedList(Collections.singletonList(newA));
        final ObjectChangeSet chSet = ChangeSetFactory.createObjectChangeSet(entityC, c, defaultDescriptor);
        chSet.addChangeRecord(
                new ChangeRecord(metamodelMocks.forOwlClassC().referencedListAtt(), c.getReferencedList()));

        builder.mergeChanges(chSet);
        assertNotNull(entityC.getReferencedList());
        assertEquals(1, entityC.getReferencedList().size());
        assertEquals(newA, entityC.getReferencedList().get(0));
    }

    @Test
    public void testBuildCloneWithMultipleWrapperTypesAndStringKey() {
        defaultLoadStateDescriptor(entityM, metamodelMocks.forOwlClassM().entityType());
        final OWLClassM m = (OWLClassM) builder.buildClone(entityM, new CloneConfiguration(defaultDescriptor, false));
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
        defaultLoadStateDescriptor(entityM, metamodelMocks.forOwlClassM().entityType());
        final OWLClassM m = (OWLClassM) builder.buildClone(entityM, new CloneConfiguration(defaultDescriptor, false));
        assertNotSame(entityM, m);
        final ObjectChangeSet changeSet = ChangeSetFactory.createObjectChangeSet(entityM, m, defaultDescriptor);
        m.setBooleanAttribute(!m.getBooleanAttribute());
        changeSet.addChangeRecord(
                new ChangeRecord(metamodelMocks.forOwlClassM().booleanAttribute(), m.getBooleanAttribute()));
        m.setIntAttribute(11111);
        changeSet.addChangeRecord(
                new ChangeRecord(metamodelMocks.forOwlClassM().integerAttribute(), m.getIntAttribute()));
        m.setLongAttribute(999L);
        changeSet.addChangeRecord(
                new ChangeRecord(metamodelMocks.forOwlClassM().longAttribute(), m.getLongAttribute()));
        m.setDoubleAttribute(1.1);
        changeSet.addChangeRecord(
                new ChangeRecord(metamodelMocks.forOwlClassM().doubleAttribute(), m.getDoubleAttribute()));
        m.setDateAttribute(new Date(System.currentTimeMillis() + 10000L));
        changeSet.addChangeRecord(
                new ChangeRecord(metamodelMocks.forOwlClassM().dateAttribute(), m.getDateAttribute()));
        m.setCharacterAttribute('w');
        changeSet.addChangeRecord(
                new ChangeRecord(metamodelMocks.forOwlClassM().characterAttribute(), m.getCharacterAttribute()));

        builder.mergeChanges(changeSet);
        assertEquals(m.getBooleanAttribute(), entityM.getBooleanAttribute());
        assertEquals(m.getIntAttribute(), entityM.getIntAttribute());
        assertEquals(m.getLongAttribute(), entityM.getLongAttribute());
        assertEquals(m.getDoubleAttribute(), entityM.getDoubleAttribute());
        assertEquals(m.getDateAttribute(), entityM.getDateAttribute());
        assertEquals(m.getCharacterAttribute(), entityM.getCharacterAttribute());
    }

    @Test
    public void cloningSkipsTransientFields() {
        final OWLClassO entityO = new OWLClassO();
        entityO.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies#entityO"));
        entityO.setStringAttribute("String");
        entityO.setTransientField("Transient");
        entityO.setTransientFieldWithAnnotation("TransientAnnotated");
        defaultLoadStateDescriptor(entityO, metamodelMocks.forOwlClassO().entityType());

        final OWLClassO clone = (OWLClassO) builder.buildClone(entityO, new CloneConfiguration(defaultDescriptor, false));
        assertNotNull(clone);
        assertEquals(entityO.getUri(), clone.getUri());
        assertEquals(entityO.getStringAttribute(), clone.getStringAttribute());
        assertNull(clone.getTransientField());
        assertNull(clone.getTransientFieldWithAnnotation());
    }

    @Test
    public void reusesAlreadyClonedInstancesWhenCloningCollections() {
        defaultLoadStateDescriptor(entityA, metamodelMocks.forOwlClassA().entityType());
        final OWLClassA cloneA = (OWLClassA) builder.buildClone(entityA, new CloneConfiguration(defaultDescriptor, false));
        entityC.setReferencedList(new ArrayList<>());
        entityC.getReferencedList().add(entityA);
        defaultLoadStateDescriptor(entityC, metamodelMocks.forOwlClassC().entityType());

        final OWLClassC cloneC = (OWLClassC) builder.buildClone(entityC, new CloneConfiguration(defaultDescriptor, false));
        assertNotNull(cloneC.getReferencedList());
        assertEquals(1, cloneC.getReferencedList().size());
        assertSame(cloneA, cloneC.getReferencedList().get(0));
    }

    @Test
    public void cloneBuildingHandlesCyclesInObjectGraphByRegisteringAlreadyVisitedObjects() {
        final OWLClassG entityG = initGWithBackwardReference();
        defaultLoadStateDescriptor(entityG, metamodelMocks.forOwlClassG().entityType());
        defaultLoadStateDescriptor(entityG.getOwlClassH(), metamodelMocks.forOwlClassH().entityType());

        final OWLClassG cloneG = (OWLClassG) builder.buildClone(entityG, new CloneConfiguration(defaultDescriptor, false));
        assertNotNull(cloneG);
        assertNotNull(cloneG.getOwlClassH());
        assertSame(cloneG, cloneG.getOwlClassH().getOwlClassG());
    }

    private OWLClassG initGWithBackwardReference() {
        final OWLClassG g = new OWLClassG(URI.create(Vocabulary.INDIVIDUAL_BASE + "entityG"));
        final OWLClassH h = new OWLClassH(URI.create(Vocabulary.INDIVIDUAL_BASE + "entityH"));
        g.setOwlClassH(h);
        h.setOwlClassG(g);
        return g;
    }

    @Test
    public void testBuildCloneOfInstanceWithArrayAsListFieldValue() {
        final OWLClassC instance = new OWLClassC(URI.create("http://test"));
        final OWLClassA another = new OWLClassA(URI.create("http://anotherA"));
        instance.setSimpleList(Arrays.asList(entityA, another));
        defaultLoadStateDescriptor(instance, metamodelMocks.forOwlClassC().entityType());
        defaultLoadStateDescriptor(entityA, metamodelMocks.forOwlClassA().entityType());
        defaultLoadStateDescriptor(another, metamodelMocks.forOwlClassA().entityType());

        final OWLClassC result = (OWLClassC) builder.buildClone(instance, new CloneConfiguration(defaultDescriptor, false));
        assertNotNull(result);
        for (int i = 0; i < instance.getSimpleList().size(); i++) {
            assertEquals(instance.getSimpleList().get(i).getUri(), result.getSimpleList().get(i).getUri());
        }
    }

    @Test
    public void mergeOfFieldOfManagedTypeUsesOriginalValueForMerge() {
        defaultLoadStateDescriptor(entityD, metamodelMocks.forOwlClassD().entityType());
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

        final ObjectChangeSet chSet = ChangeSetFactory.createObjectChangeSet(entityD, dClone, new EntityDescriptor());
        chSet.addChangeRecord(new ChangeRecord(metamodelMocks.forOwlClassD().owlClassAAtt(), newValueClone));
        when(uow.getOriginal(newValueClone)).thenReturn(newValue);
        builder.mergeChanges(chSet);
        assertSame(newValue, entityD.getOwlClassA());
        assertEquals(newValue.getUri(), entityD.getOwlClassA().getUri());
        assertEquals(newValue.getStringAttribute(), entityD.getOwlClassA().getStringAttribute());
    }

    @Test
    public void buildCloneClonesMappedSuperclassFieldsAsWell() {
        defaultLoadStateDescriptor(entityQ, metamodelMocks.forOwlClassQ().entityType());
        defaultLoadStateDescriptor(entityA, metamodelMocks.forOwlClassA().entityType());
        final OWLClassQ clone = (OWLClassQ) builder.buildClone(entityQ, new CloneConfiguration(defaultDescriptor, false));
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
        defaultLoadStateDescriptor(entityQ, metamodelMocks.forOwlClassQ().entityType());
        final OWLClassQ qClone = new OWLClassQ();
        qClone.setUri(entityQ.getUri());
        qClone.setStringAttribute("newStringAtt");
        qClone.setParentString("anotherStringAtt");
        final OWLClassA newA = new OWLClassA();
        newA.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/newA"));
        qClone.setOwlClassA(newA);
        final ObjectChangeSet changeSet = ChangeSetFactory.createObjectChangeSet(entityQ, qClone, new EntityDescriptor());
        changeSet.addChangeRecord(
                new ChangeRecord(metamodelMocks.forOwlClassQ().qStringAtt(), qClone.getStringAttribute()));
        changeSet.addChangeRecord(
                new ChangeRecord(metamodelMocks.forOwlClassQ().qParentStringAtt(), qClone.getParentString()));
        changeSet.addChangeRecord(
                new ChangeRecord(metamodelMocks.forOwlClassQ().qOwlClassAAtt(), qClone.getOwlClassA()));

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
            CloneRegistrationDescriptor desc = (CloneRegistrationDescriptor) invocation.getArguments()[1];
            final Object clone = builder.buildClone(obj, new CloneConfiguration(desc.getDescriptor(), false));
            // THIS is the important verification
            assertNotNull(EntityPropertiesUtils.getIdentifier(clone, metamodel));
            return clone;
        }).when(uow).registerExistingObject(any(), any(CloneRegistrationDescriptor.class));
        defaultLoadStateDescriptor(b, metamodel.entity(B.class));
        defaultLoadStateDescriptor(a, metamodel.entity(A.class));

        final B result = (B) builder.buildClone(b, new CloneConfiguration(defaultDescriptor, false));
        assertNotNull(result);
        assertNotNull(result.a);
        assertEquals(result, result.a.b.iterator().next());
        assertEquals(b.uri, result.uri);
        assertEquals(a.uri, result.a.uri);
    }

    private void initMetamodelForAB() throws Exception {
        when(uow.isEntityType(A.class)).thenReturn(true);
        when(uow.isEntityType(B.class)).thenReturn(true);
        final IdentifiableEntityType<A> etA = mock(IdentifiableEntityType.class);
        when(etA.getJavaType()).thenReturn(A.class);
        final IdentifiableEntityType<B> etB = mock(IdentifiableEntityType.class);
        when(etB.getJavaType()).thenReturn(B.class);
        final Identifier idA = new IRIIdentifierImpl<>(etA, A.class.getDeclaredField("uri"), true);
        final Identifier idB = new IRIIdentifierImpl<>(etB, B.class.getDeclaredField("uri"), true);
        when(etA.getIdentifier()).thenReturn(idA);
        when(etB.getIdentifier()).thenReturn(idB);
        final PluralAttribute hasB = mock(PluralAttribute.class);
        when(hasB.getJavaField()).thenReturn(A.class.getDeclaredField("b"));
        when(hasB.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.OBJECT);
        when(hasB.getCollectionType()).thenReturn(CollectionType.SET);
        when(hasB.getElementType()).thenReturn(etB);
        when(hasB.isCollection()).thenReturn(true);
        final Set aFieldSpecs = new HashSet<>(2);
        aFieldSpecs.add(idA);
        aFieldSpecs.add(hasB);
        when(etA.getFieldSpecifications()).thenReturn(aFieldSpecs);
        when(etA.getFieldSpecification("b")).thenReturn(hasB);
        final SingularAttribute hasA = mock(SingularAttribute.class);
        when(hasA.getJavaField()).thenReturn(B.class.getDeclaredField("a"));
        when(hasA.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.OBJECT);
        when(hasA.getJavaType()).thenReturn(A.class);
        final Set bFieldSpecs = new HashSet<>(2);
        bFieldSpecs.add(idB);
        bFieldSpecs.add(hasA);
        when(etB.getFieldSpecifications()).thenReturn(bFieldSpecs);
        when(etB.getFieldSpecification("a")).thenReturn(hasA);
        doReturn(etA).when(metamodel).entity(A.class);
        doReturn(etB).when(metamodel).entity(B.class);
    }

    public static class A {
        @Id
        private URI uri;

        @OWLObjectProperty(iri = Vocabulary.ATTRIBUTE_BASE + "hasB")
        private Set<B> b;
    }

    public static class B {
        @Id
        private URI uri;

        @OWLObjectProperty(iri = Vocabulary.ATTRIBUTE_BASE + "hasA")
        private A a;
    }

    @Test
    public void buildCloneReturnsSameInstanceForImmutableClasses() {
        final LocalDateTime instance = LocalDateTime.now();
        assertSame(instance, builder.buildClone(instance, new CloneConfiguration(defaultDescriptor, false)));
    }

    @Test
    public void buildCloneReturnsSameInstanceForJava8Instant() {
        final Instant instance = Instant.now();
        assertSame(instance, builder.buildClone(instance, new CloneConfiguration(defaultDescriptor, false)));
    }

    @Test
    void buildCloneCreatesAndRegistersLoadStateDescriptorForClone() {
        loadStateRegistry.put(entityA, LoadStateDescriptorFactory.createAllLoaded(entityA, metamodelMocks.forOwlClassA()
                                                                                           .entityType()));
        final OWLClassA res = (OWLClassA) builder.buildClone(entityA, new CloneConfiguration(defaultDescriptor, true));
        assertTrue(uow.getLoadStateRegistry().contains(res));
        final LoadStateDescriptor<OWLClassA> originalLoadState = loadStateRegistry.get(entityA);
        final LoadStateDescriptor<OWLClassA> resultLoadState = loadStateRegistry.get(res);
        metamodelMocks.forOwlClassA().entityType().getFieldSpecifications()
                      .forEach(fs -> assertEquals(originalLoadState.isLoaded(fs), resultLoadState.isLoaded(fs)));
    }

    @Test
    void buildCloneSetsLazyLoadingProxiesForPluralAttributesWithNotLoadedState() {
        final LoadStateDescriptor<OWLClassC> loadState = LoadStateDescriptorFactory.createAllUnknown(entityC, metamodelMocks.forOwlClassC()
                                                                                                                            .entityType());
        loadStateRegistry.put(entityC, loadState);
        loadState.setLoaded(metamodelMocks.forOwlClassC().referencedListAtt(), LoadState.LOADED);
        loadState.setLoaded(metamodelMocks.forOwlClassC().simpleListAtt(), LoadState.NOT_LOADED);

        final OWLClassC result = (OWLClassC) builder.buildClone(entityC, new CloneConfiguration(defaultDescriptor, true));
        assertNotNull(result.getSimpleList());
        assertInstanceOf(LazyLoadingProxy.class, result.getSimpleList());
    }

    @Test
    void buildCloneSetsLazyLoadingProxiesForSingularAttributesWithNotLoadedState() {
        when(metamodelMocks.forOwlClassD().owlClassAAtt().getFetchType()).thenReturn(FetchType.LAZY);
        final LoadStateDescriptor<OWLClassD> loadState = LoadStateDescriptorFactory.createAllUnknown(entityD, metamodelMocks.forOwlClassD()
                                                                                                                            .entityType());
        loadStateRegistry.put(entityD, loadState);
        entityD.setOwlClassA(null);
        loadState.setLoaded(metamodelMocks.forOwlClassD().owlClassAAtt(), LoadState.LOADED);

        final OWLClassD result = (OWLClassD) builder.buildClone(entityD, new CloneConfiguration(defaultDescriptor, true));
        assertNull(result.getOwlClassA());
    }

    @Test
    public void mergeChangesUpdatesLoadStateOfOriginalLoadStateDescriptor() {
        final LoadStateDescriptor<OWLClassC> loadStateDescriptor = LoadStateDescriptorFactory.createNotLoaded(entityC, metamodelMocks.forOwlClassC()
                                                                                                                      .entityType());
        loadStateDescriptor.setLoaded(metamodelMocks.forOwlClassC().referencedListAtt(), LoadState.LOADED);
        loadStateRegistry.put(entityC, loadStateDescriptor);
        final OWLClassC c = (OWLClassC) builder.buildClone(entityC, new CloneConfiguration(defaultDescriptor, false));
        assertNotSame(entityC, c);
        c.setSimpleList(null);
        final ObjectChangeSet chSet = ChangeSetFactory.createObjectChangeSet(entityC, c, defaultDescriptor);
        chSet.addChangeRecord(new ChangeRecord(metamodelMocks.forOwlClassC().simpleListAtt(),
                c.getSimpleList()));
        builder.mergeChanges(chSet);

        assertNull(entityC.getSimpleList());
        assertEquals(LoadState.LOADED, loadStateDescriptor.isLoaded(metamodelMocks.forOwlClassC().simpleListAtt()));
    }
}

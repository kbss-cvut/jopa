/*
 * JOPA
 * Copyright (C) 2023 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.sessions.change;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassB;
import cz.cvut.kbss.jopa.environment.OWLClassC;
import cz.cvut.kbss.jopa.environment.OWLClassD;
import cz.cvut.kbss.jopa.environment.OWLClassE;
import cz.cvut.kbss.jopa.environment.OWLClassF;
import cz.cvut.kbss.jopa.environment.OWLClassK;
import cz.cvut.kbss.jopa.environment.OWLClassM;
import cz.cvut.kbss.jopa.environment.OWLClassO;
import cz.cvut.kbss.jopa.environment.OWLClassQ;
import cz.cvut.kbss.jopa.environment.Vocabulary;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.environment.utils.TestEnvironmentUtils;
import cz.cvut.kbss.jopa.model.MetamodelImpl;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.proxy.lazy.LazyLoadingListProxy;
import cz.cvut.kbss.jopa.proxy.lazy.gen.LazyLoadingEntityProxyGenerator;
import cz.cvut.kbss.jopa.sessions.MetamodelProvider;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.Set;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasItem;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class ChangeCalculatorTest {

    private static final URI DEFAULT_CONTEXT = URI.create("http://defaultContext");

    private OWLClassA testA;
    private OWLClassB testB;
    private OWLClassD testD;
    private OWLClassC testC;
    private OWLClassO testO;
    private OWLClassM testM;
    private OWLClassQ testQ;

    private OWLClassA testAClone;
    private OWLClassB testBClone;
    private OWLClassC testCClone;
    private OWLClassD testDClone;

    private Set<String> typesCollection;

    @Mock
    private MetamodelProvider providerMock;
    @Mock
    private MetamodelImpl metamodelMock;

    private MetamodelMocks metamodelMocks;

    private ChangeCalculator sut;

    @BeforeEach
    public void setup() throws Exception {
        initInstances();
        sut = new ChangeCalculator(providerMock);
        when(providerMock.isEntityType(any(Class.class))).thenAnswer(invocation -> {
            final Class<?> cls = (Class<?>) invocation.getArguments()[0];
            return TestEnvironmentUtils.getManagedTypes().contains(cls);
        });
        when(providerMock.getMetamodel()).thenReturn(metamodelMock);
        this.metamodelMocks = new MetamodelMocks();
        metamodelMocks.setMocks(metamodelMock);
        typesCollection = Generators.generateTypes(10);
        initClones();
        final List<OWLClassA> clonedList = new ArrayList<>(testC.getReferencedList().size());
        for (OWLClassA a : testC.getReferencedList()) {
            final OWLClassA newA = new OWLClassA();
            newA.setUri(a.getUri());
            newA.setStringAttribute(a.getStringAttribute());
            clonedList.add(newA);
        }
        testCClone.setReferencedList(clonedList);
    }

    private void initInstances() {
        this.testA = Generators.generateOwlClassAInstance();
        initB();
        initC();
        initD();
        this.testM = new OWLClassM();
        testM.initializeTestValues(false);
        initO();
        initQ();
    }

    private void initB() {
        testB = new OWLClassB();
        testB.setUri(Generators.createIndividualIdentifier());
        testB.setStringAttribute("someString");
        final Map<String, Set<String>> props = new HashMap<>();
        props.put("propertyOne", Collections.singleton("valueOne"));
        props.put("propertyTwo", Collections.singleton("valueTwo"));
        final Set<String> multiProp = new HashSet<>();
        multiProp.add("valueThree");
        multiProp.add("valueFour");
        props.put("propertyThree", multiProp);
        testB.setProperties(props);
    }

    private void initC() {
        testC = new OWLClassC(Generators.createIndividualIdentifier());
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

    private void initD() {
        this.testD = new OWLClassD(Generators.createIndividualIdentifier());
        testD.setOwlClassA(Generators.generateOwlClassAInstance());
    }

    private void initO() {
        this.testO = new OWLClassO(Generators.createIndividualIdentifier());
        testO.setStringAttribute("String");
    }

    private void initQ() {
        this.testQ = new OWLClassQ();
        testQ.setUri(Generators.createIndividualIdentifier());
        testQ.setStringAttribute("someString");
        testQ.setParentString("parentString");
        testQ.setLabel("label");
        testQ.setOwlClassA(testA);
    }

    private void initClones() {
        this.testAClone = new OWLClassA(testA.getUri());
        testAClone.setStringAttribute(testA.getStringAttribute());
        testAClone.setTypes(new HashSet<>(testA.getTypes()));
        final Map<String, Set<String>> cloneProps = new HashMap<>();
        for (Entry<String, Set<String>> e : testB.getProperties().entrySet()) {
            final Set<String> set = new HashSet<>(e.getValue());
            cloneProps.put(e.getKey(), set);
        }
        testBClone = new OWLClassB(testB.getUri());
        testBClone.setStringAttribute(testB.getStringAttribute());
        testBClone.setProperties(cloneProps);
        testCClone = new OWLClassC();
        testCClone.setUri(testC.getUri());
        testDClone = new OWLClassD(testD.getUri());
        final OWLClassA refAClone = new OWLClassA(testD.getOwlClassA().getUri());
        refAClone.setStringAttribute(testD.getOwlClassA().getStringAttribute());
        refAClone.setTypes(new HashSet<>(testD.getOwlClassA().getTypes()));
        testDClone.setOwlClassA(refAClone);
    }

    @Test
    public void hasChangesOnNullReturnsFalse() {
        assertFalse(sut.hasChanges(null, null));
    }

    @Test
    public void hasChangeWithChangedDataPropertyValueReturnsTrue() {
        testAClone.setStringAttribute("differentStringAttribute");
        assertTrue(sut.hasChanges(testA, testAClone));
    }

    @Test
    public void hasChangeWithoutChangeOnDataPropertyReturnsFalse() {
        testAClone.setStringAttribute(testA.getStringAttribute());
        assertFalse(sut.hasChanges(testA, testA));
    }

    @Test
    public void hasChangeOnDifferentObjectPropertyValueReturnsTrue() {
        final OWLClassA ref = new OWLClassA(Generators.createIndividualIdentifier());
        ref.setStringAttribute(testA.getStringAttribute());
        testDClone.setOwlClassA(ref);
        assertTrue(sut.hasChanges(testD, testDClone));
    }

    @Test
    public void hasChangeFromEmptyCollectionToNonEmptyReturnsTrue() {
        testAClone.setTypes(typesCollection);
        testA.setTypes(new HashSet<>());
        assertTrue(sut.hasChanges(testA, testAClone));
    }

    @Test
    public void hasChangeWhenAddedItemToCollectionReturnsTrue() {
        testA.setTypes(typesCollection);
        Set<String> changed = new HashSet<>();
        Iterator<String> it = typesCollection.iterator();
        it.next();
        changed.add("111");
        while (it.hasNext()) {
            changed.add(it.next());
        }
        testAClone.setTypes(changed);
        assertTrue(sut.hasChanges(testA, testAClone));
    }

    @Test
    public void hasChangeWhenItemRemovedFromCollectionReturnsTrue() {
        final Iterator<OWLClassA> it = testCClone.getReferencedList().iterator();
        boolean removed = false;
        while (it.hasNext()) {
            it.next();
            if (Generators.randomBoolean() || !removed) {
                it.remove();
                removed = true;
            }
        }
        assertTrue(sut.hasChanges(testC, testCClone));
    }

    @Test
    public void hasChangeOnDataPropertyWhenOriginalValueWasNullReturnsTrue() {
        testA.setStringAttribute(null);
        testAClone.setStringAttribute("someString");
        assertTrue(sut.hasChanges(testA, testAClone));
    }

    @Test
    public void hasChangeAttributeValueChangeOnReferenceReturnsFalse() {
        testDClone.getOwlClassA().setStringAttribute("updatedString");
        assertFalse(sut.hasChanges(testD, testDClone));
    }

    @Test
    public void calculateChangesThrowsNPXForNullArgument() {
        assertThrows(NullPointerException.class, () -> sut.calculateChanges(null));
    }

    @Test
    public void calculateChangesForIdenticalObjectsAddsNoChangesToChangeSet() {
        final ObjectChangeSet chSet = createChangeSet(testA, testAClone);
        final boolean res = sut.calculateChanges(chSet);
        assertFalse(res);
        assertTrue(chSet.getChanges().isEmpty());
    }

    @Test
    public void calculateChangesRegistersChangeOnStringAttribute() {
        testAClone.setStringAttribute("updated");
        ObjectChangeSet chSet = createChangeSet(testA, testAClone);
        assertTrue(chSet.getChanges().isEmpty());
        final boolean res = sut.calculateChanges(chSet);
        assertTrue(res);
        assertFalse(chSet.getChanges().isEmpty());
        verifyChangeSetContainsChangeOfAttribute(metamodelMocks.forOwlClassA().stringAttribute(), chSet);
    }

    private void verifyChangeSetContainsChangeOfAttribute(FieldSpecification<?, ?> att, ObjectChangeSet changeSet) {
        final Optional<ChangeRecord> result = changeSet.getChanges().stream()
                                                       .filter(ch -> ch.getAttribute().equals(att)).findAny();
        assertTrue(result.isPresent());
    }

    @Test
    public void calculateChangesRegistersChangeOnPrimitiveTypeAttribute() {
        final OWLClassM testMClone = new OWLClassM();
        testMClone.setIntAttribute(testM.getIntAttribute() + 117);
        final ObjectChangeSet chSet = createChangeSet(testM, testMClone);
        assertTrue(chSet.getChanges().isEmpty());
        final boolean res = sut.calculateChanges(chSet);
        assertTrue(res);
        assertFalse(chSet.getChanges().isEmpty());
        verifyChangeSetContainsChangeOfAttribute(metamodelMocks.forOwlClassM().integerAttribute(), chSet);
    }

    @Test
    public void calculateChangesRegistersChangeOnObjectPropertyAttribute() {
        testDClone.setOwlClassA(testAClone);
        final ObjectChangeSet chSet = createChangeSet(testD, testDClone);
        assertTrue(chSet.getChanges().isEmpty());
        final boolean res = sut.calculateChanges(chSet);
        assertTrue(res);
        assertEquals(1, chSet.getChanges().size());
        assertEquals(testAClone, chSet.getChanges().iterator().next().getNewValue());
    }

    @Test
    public void calculateChangesRegistersChangeInCollection() {
        testA.setTypes(typesCollection);
        Set<String> newCollection = new HashSet<>(typesCollection);
        newCollection.remove(typesCollection.iterator().next());
        newCollection.add("String");
        testAClone.setTypes(newCollection);
        final ObjectChangeSet chSet = createChangeSet(testA, testAClone);
        assertTrue(chSet.getChanges().isEmpty());
        final boolean res = sut.calculateChanges(chSet);
        assertTrue(res);
        assertEquals(1, chSet.getChanges().size());
        assertThat((Set<String>) chSet.getChanges().iterator().next().getNewValue(), hasItem("String"));
    }

    @Test
    public void calculateChangesRegistersMultipleChanges() {
        testA.setTypes(typesCollection);
        Set<String> newCollection = new HashSet<>(typesCollection);
        newCollection.remove(typesCollection.iterator().next());
        newCollection.add("String");
        testAClone.setTypes(newCollection);
        testAClone.setStringAttribute("AnotherStringAttribute");
        final ObjectChangeSet chSet = createChangeSet(testA, testAClone);
        assertTrue(chSet.getChanges().isEmpty());
        final boolean res = sut.calculateChanges(chSet);
        assertTrue(res);
        assertEquals(2, chSet.getChanges().size());
        verifyChangeSetContainsChangeOfAttribute(metamodelMocks.forOwlClassA().stringAttribute(), chSet);
        verifyChangeSetContainsChangeOfAttribute(metamodelMocks.forOwlClassA().typesSpec(), chSet);
    }

    @Test
    public void calculateChangesRegistersChangeWhenValueIsSetToNull() {
        final ObjectChangeSet chSet = createChangeSet(testA, testAClone);
        testAClone.setStringAttribute(null);
        assertTrue(chSet.getChanges().isEmpty());
        final boolean res = sut.calculateChanges(chSet);
        assertTrue(res);
        assertFalse(chSet.getChanges().isEmpty());
        assertNotNull(chSet);
        assertNull(chSet.getChanges().iterator().next().getNewValue());
    }

    @Test
    public void calculateChangesRegistersChangeWhenValueIsSetToNonNull() {
        testA.setTypes(null);
        final ObjectChangeSet chSet = createChangeSet(testA, testAClone);
        final boolean res = sut.calculateChanges(chSet);
        assertTrue(res);
        assertEquals(1, chSet.getChanges().size());
        assertNotNull(chSet.getChanges().iterator().next().getNewValue());
    }

    @Test
    public void calculateChangesRegistersItemRemovalFromReferenceCollection() {
        testCClone.getReferencedList().remove(4);
        final ObjectChangeSet chSet = createChangeSet(testC, testCClone);
        assertTrue(chSet.getChanges().isEmpty());
        final boolean res = sut.calculateChanges(chSet);
        assertTrue(res);
        verifyChangeSetContainsChangeOfAttribute(metamodelMocks.forOwlClassC().referencedListAtt(), chSet);
        assertEquals(1, chSet.getChanges().size());
    }

    @SuppressWarnings("unchecked")
    @Test
    public void calculateChangesRegistersItemAdditionToReferenceCollection() {
        testCClone.getReferencedList().add(testA);
        final ObjectChangeSet chSet = createChangeSet(testC, testCClone);
        assertTrue(chSet.getChanges().isEmpty());
        final boolean res = sut.calculateChanges(chSet);
        assertTrue(res);
        assertEquals(1, chSet.getChanges().size());
        final ChangeRecord r = chSet.getChanges().iterator().next();
        List<OWLClassA> refs = (List<OWLClassA>) r.getNewValue();
        assertEquals(testA.getUri(), refs.get(10).getUri());
        assertEquals(testA.getStringAttribute(), refs.get(10).getStringAttribute());
    }

    @SuppressWarnings("unchecked")
    @Test
    public void calculateChangesRegistersItemReplacementInReferenceCollection() {
        testCClone.getReferencedList().remove(3);
        testCClone.getReferencedList().add(testA);
        final ObjectChangeSet chSet = createChangeSet(testC, testCClone);
        assertTrue(chSet.getChanges().isEmpty());
        final boolean res = sut.calculateChanges(chSet);
        assertTrue(res);
        verifyChangeSetContainsChangeOfAttribute(metamodelMocks.forOwlClassC().referencedListAtt(), chSet);
        assertEquals(1, chSet.getChanges().size());
        final ChangeRecord r = chSet.getChanges().iterator().next();
        List<OWLClassA> refs = (List<OWLClassA>) r.getNewValue();
        assertTrue(refs.contains(testA));
    }

    @Test
    public void calculateChangesRegistersMapKeyAddition() {
        testBClone.getProperties().put("newProperty", Collections.singleton("propVal"));
        final ObjectChangeSet chSet = createChangeSet(testB, testBClone);
        final boolean res = sut.calculateChanges(chSet);
        assertTrue(res);
        assertEquals(1, chSet.getChanges().size());
        verifyChangeSetContainsChangeOfAttribute(metamodelMocks.forOwlClassB().propertiesSpec(), chSet);
    }

    @Test
    public void calculateChangesRegistersMapValueChange() {
        final String key = testB.getProperties().keySet().iterator().next();
        testBClone.getProperties().put(key, Collections.singleton("propVal"));
        final ObjectChangeSet chSet = createChangeSet(testB, testBClone);
        final boolean res = sut.calculateChanges(chSet);
        assertTrue(res);
        assertEquals(1, chSet.getChanges().size());
        verifyChangeSetContainsChangeOfAttribute(metamodelMocks.forOwlClassB().propertiesSpec(), chSet);
    }

    @Test
    public void calculateChangesRegistersMapValueAddition() {
        final String key = testB.getProperties().keySet().iterator().next();
        testBClone.getProperties().get(key).add("addedValue");
        final ObjectChangeSet chSet = createChangeSet(testB, testBClone);
        final boolean res = sut.calculateChanges(chSet);
        assertTrue(res);
        assertEquals(1, chSet.getChanges().size());
        verifyChangeSetContainsChangeOfAttribute(metamodelMocks.forOwlClassB().propertiesSpec(), chSet);
    }

    private ObjectChangeSet createChangeSet(Object orig, Object clone) {
        return ChangeSetFactory.createObjectChangeSet(orig, clone, new EntityDescriptor(DEFAULT_CONTEXT));
    }

    @Test
    public void twoSetsWithSameElementsButDifferentOrderHaveNoChanges() {
        testA.setTypes(typesCollection);
        testAClone.setStringAttribute(testA.getStringAttribute());
        final List<String> lst = new ArrayList<>(typesCollection);
        Collections.reverse(lst);
        final Set<String> newTypes = new LinkedHashSet<>(lst);
        testAClone.setTypes(newTypes);
        assertNotEquals(typesCollection.iterator().next(), newTypes.iterator().next());
        final boolean res = sut.hasChanges(testA, testAClone);
        assertFalse(res);
    }

    @Test
    public void twoSetsWithManagedElementsWithSameIdentifiersHaveNoChanges() {
        final OWLClassF testF = new OWLClassF();
        final OWLClassF cloneF = new OWLClassF();
        initFAndClone(testF, cloneF);
        final boolean res = sut.hasChanges(testF, cloneF);
        assertFalse(res);
    }

    private void initFAndClone(OWLClassF orig, OWLClassF clone) {
        final URI uri = URI.create(Vocabulary.INDIVIDUAL_BASE + "testF");
        orig.setUri(uri);
        clone.setUri(uri);
        orig.setSimpleSet(new HashSet<>());
        clone.setSimpleSet(new HashSet<>());
        orig.getSimpleSet().add(testA);
        testAClone.setStringAttribute(testA.getStringAttribute());
        clone.getSimpleSet().add(testAClone);
        final URI aUri = URI.create(Vocabulary.INDIVIDUAL_BASE + "testAA");
        final OWLClassA extraA = new OWLClassA(aUri);
        extraA.setStringAttribute("string");
        orig.getSimpleSet().add(extraA);
        final OWLClassA extraAClone = new OWLClassA(aUri);
        extraAClone.setStringAttribute(extraA.getStringAttribute());
        clone.getSimpleSet().add(extraAClone);
    }

    @Test
    public void twoSetsWithManagedElementsOneElementReplacedWithNewWithoutIdHaveChanges() {
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
        final boolean res = sut.hasChanges(testF, cloneF);
        assertTrue(res);
    }

    @Test
    public void hasChangesIgnoresTransientFieldChanges() {
        final OWLClassO testOClone = new OWLClassO();
        testOClone.setUri(testO.getUri());
        testOClone.setStringAttribute(testO.getStringAttribute());
        testOClone.setTransientField("Change!");

        final boolean res = sut.hasChanges(testO, testOClone);
        assertFalse(res);
    }

    @Test
    public void calculateChangesIgnoresTransientFieldChanges() {
        final OWLClassO testOClone = new OWLClassO();
        testOClone.setUri(testO.getUri());
        testOClone.setStringAttribute(testO.getStringAttribute());
        testOClone.setTransientFieldWithAnnotation("Change!");

        final ObjectChangeSet changeSet = createChangeSet(testO, testOClone);
        final boolean res = sut.calculateChanges(changeSet);
        assertFalse(res);
        assertTrue(changeSet.getChanges().isEmpty());
    }

    @Test
    public void calculateChangesDetectsChangesInMappedSuperclassFields() {
        final OWLClassQ testQClone = new OWLClassQ();
        testQClone.setUri(testQ.getUri());
        testQClone.setLabel("differentLabel");
        testQClone.setParentString("differentParentString");
        testQClone.setStringAttribute("differentString");
        testQClone.setOwlClassA(testA);

        final ObjectChangeSet changeSet = createChangeSet(testQ, testQClone);
        final boolean res = sut.calculateChanges(changeSet);
        assertTrue(res);
        final Set<ChangeRecord> changes = changeSet.getChanges();
        assertEquals(3, changes.size());
        verifyChangeSetContainsChangeOfAttribute(metamodelMocks.forOwlClassQ().qLabelAtt(), changeSet);
        verifyChangeSetContainsChangeOfAttribute(metamodelMocks.forOwlClassQ().qStringAtt(), changeSet);
        verifyChangeSetContainsChangeOfAttribute(metamodelMocks.forOwlClassQ().qParentStringAtt(), changeSet);
        changes.stream().filter(ch -> ch.getAttribute().equals(metamodelMocks.forOwlClassQ().qLabelAtt()))
               .forEach(ch -> assertEquals(testQClone.getLabel(), ch.getNewValue()));
        changes.stream().filter(ch -> ch.getAttribute().equals(metamodelMocks.forOwlClassQ().qStringAtt()))
               .forEach(ch -> assertEquals(testQClone.getStringAttribute(), ch.getNewValue()));
        changes.stream().filter(ch -> ch.getAttribute().equals(metamodelMocks.forOwlClassQ().qParentStringAtt()))
               .forEach(ch -> assertEquals(testQClone.getParentString(), ch.getNewValue()));
    }

    @Test
    public void calculateChangesDetectsChangesInMappedSuperclassObjectPropertyField() {
        final OWLClassQ testQClone = new OWLClassQ();
        testQClone.setUri(testQ.getUri());
        testQClone.setStringAttribute(testQ.getStringAttribute());
        testQClone.setParentString(testQ.getParentString());
        testQClone.setLabel(testQ.getLabel());
        final OWLClassA newA = new OWLClassA();
        newA.setUri(URI.create(Vocabulary.INDIVIDUAL_BASE + "newA"));
        testQClone.setOwlClassA(newA);

        final ObjectChangeSet changeSet = createChangeSet(testQ, testQClone);
        final boolean res = sut.calculateChanges(changeSet);
        assertTrue(res);
        final Set<ChangeRecord> changes = changeSet.getChanges();
        assertEquals(1, changes.size());
        verifyChangeSetContainsChangeOfAttribute(metamodelMocks.forOwlClassQ().qOwlClassAAtt(), changeSet);
        assertEquals(newA, changes.iterator().next().getNewValue());
    }

    @Test
    void calculateChangesDoesNotDetectChangeWhenOriginalValueIsNullAndCloneValueIsLazyLoadingProxy() throws Exception {
        final OWLClassK original = new OWLClassK(Generators.createIndividualIdentifier());
        final OWLClassK clone = new OWLClassK(original.getUri());
        final LazyLoadingEntityProxyGenerator lazyProxyGenerator = new LazyLoadingEntityProxyGenerator();
        final Class<? extends OWLClassE> lazyProxyCls = lazyProxyGenerator.generate(OWLClassE.class);
        clone.setOwlClassE(lazyProxyCls.getDeclaredConstructor().newInstance());
        final ObjectChangeSet changeSet = createChangeSet(original, clone);
        final boolean res = sut.calculateChanges(changeSet);
        assertFalse(res);
    }

    @Test
    void calculateChangesSkipsIdentifier() {
        final OWLClassA original = Generators.generateOwlClassAInstance();
        final OWLClassA clone = new OWLClassA(Generators.createIndividualIdentifier());
        clone.setStringAttribute(original.getStringAttribute());
        clone.setTypes(new HashSet<>(original.getTypes()));
        final ObjectChangeSet changeSet = createChangeSet(original, clone);
        final boolean res = sut.calculateChanges(changeSet);
        assertFalse(res);
    }

    @Test
    void hasChangesSkipsIdentifier() {
        final OWLClassA original = Generators.generateOwlClassAInstance();
        final OWLClassA clone = new OWLClassA(Generators.createIndividualIdentifier());
        clone.setStringAttribute(original.getStringAttribute());
        clone.setTypes(new HashSet<>(original.getTypes()));
        assertFalse(sut.hasChanges(original, clone));
    }

    @Test
    void calculateChangesSkipsLazyLoadedCollectionValueWhenOriginalIsEmptyCollection() {
        final OWLClassC original = new OWLClassC(Generators.createIndividualIdentifier());
        original.setSimpleList(Collections.emptyList());
        final OWLClassC clone = new OWLClassC(original.getUri());
        final UnitOfWork uow = mock(UnitOfWork.class);
        clone.setSimpleList(new LazyLoadingListProxy<>(clone, metamodelMocks.forOwlClassC().simpleListAtt(), uow));
        final ObjectChangeSet changeSet = createChangeSet(original, clone);
        assertFalse(sut.calculateChanges(changeSet));
        verify(uow, never()).loadEntityField(any(), any());
    }
}

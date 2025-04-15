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
package cz.cvut.kbss.jopa.sessions.cache;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassB;
import cz.cvut.kbss.jopa.environment.OWLClassD;
import cz.cvut.kbss.jopa.environment.OWLClassM;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.model.LoadState;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.sessions.descriptor.LoadStateDescriptor;
import org.junit.jupiter.api.Test;

import java.net.URI;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public abstract class AbstractCacheManagerTest<T extends CacheManager> {

    static final URI CONTEXT_ONE = URI.create("http://jopa-unit-tests");
    static final URI CONTEXT_TWO = URI.create("http://jopa-unit-testsTwo");

    OWLClassA testA;
    OWLClassB testB;
    private OWLClassM testM;
    private Map<URI, OWLClassB> listOfBs;

    T manager;

    public void setUp() throws Exception {
        testA = new OWLClassA(Generators.createIndividualIdentifier());
        testA.setStringAttribute("testAttribute");
        testB = new OWLClassB(Generators.createIndividualIdentifier());
        testB.setStringAttribute("stringAttribute");
        testM = new OWLClassM();
        testM.setKey("http://testM");
        listOfBs = new HashMap<>();
        for (int i = 0; i < 10; i++) {
            final URI pkI = URI.create("http://testBList_" + i);
            final OWLClassB b = new OWLClassB();
            b.setUri(pkI);
            b.setStringAttribute("Instance number " + i);
            listOfBs.put(pkI, b);
        }
    }

    Descriptor descriptor(URI context) {
        return new EntityDescriptor(context);
    }

    @Test
    public void testAddIntoContext() {
        final Descriptor descriptor = descriptor(CONTEXT_TWO);
        manager.add(testA.getUri(), testA, descriptors(descriptor));
        assertTrue(manager.contains(testA.getClass(), testA.getUri(), descriptor));
        final Object res = manager.get(testA.getClass(), testA.getUri(), descriptor);
        assertNotNull(res);
        assertSame(testA, res);
    }

    protected Descriptors descriptors(Descriptor repoDescriptor) {
        return new Descriptors(repoDescriptor, new LoadStateDescriptor<>(testA, mock(EntityType.class), LoadState.UNKNOWN));
    }

    @Test
    public void testAddToDefault() {
        final Descriptor descriptor = descriptor(null);
        manager.add(testA.getUri(), testA, descriptors(descriptor));
        assertTrue(manager.contains(testA.getClass(), testA.getUri(), descriptor));
        assertFalse(manager.contains(testA.getClass(), testA.getUri(), descriptor(CONTEXT_ONE)));
        assertFalse(manager.contains(testA.getClass(), testA.getUri(), descriptor(CONTEXT_TWO)));
        final Object res = manager.get(testA.getClass(), testA.getUri(), descriptor);
        assertNotNull(res);
        assertSame(testA, res);
    }

    @Test
    public void addNullThrowsNullPointerException() {
        assertThrows(NullPointerException.class,
                () -> manager.add(URI.create("http://blahblahblah"), null, descriptors(descriptor(null))));
    }

    @Test
    public void addingWithDuplicateIdentifierReplacesExistingRecord() {
        final Descriptor descriptor = descriptor(CONTEXT_ONE);
        manager.add(testA.getUri(), testA, descriptors(descriptor));
        final OWLClassA duplicate = new OWLClassA();
        final String newStr = testA.getStringAttribute() + "duplicated";
        duplicate.setStringAttribute(newStr);
        duplicate.setUri(testA.getUri());
        manager.add(duplicate.getUri(), duplicate, descriptors(descriptor));
        final OWLClassA res = manager.get(testA.getClass(), testA.getUri(), descriptor);
        assertNotNull(res);
        assertEquals(newStr, res.getStringAttribute());
    }

    @Test
    public void addWithSameIdToDifferentContextsRetainsBothRecords() {
        final Descriptor dOne = descriptor(CONTEXT_ONE);
        final Descriptor dTwo = descriptor(CONTEXT_TWO);
        manager.add(testA.getUri(), testA, descriptors(dOne));
        final OWLClassA duplicate = new OWLClassA();
        duplicate.setUri(testA.getUri());
        final String newStr = testA.getStringAttribute() + "duplicated";
        duplicate.setStringAttribute(newStr);
        manager.add(duplicate.getUri(), duplicate, descriptors(dTwo));
        assertTrue(manager.contains(testA.getClass(), testA.getUri(), dOne));
        assertTrue(manager.contains(duplicate.getClass(), duplicate.getUri(), dTwo));
        assertSame(testA, manager.get(testA.getClass(), testA.getUri(), dOne));
        assertSame(duplicate, manager.get(testA.getClass(), duplicate.getUri(), dTwo));
    }

    @Test
    public void testContainsDefault() {
        final Descriptor descriptor = descriptor(null);
        manager.add(testA.getUri(), testA, descriptors(descriptor));
        assertTrue(manager.contains(testA.getClass(), testA.getUri(), descriptor));
        assertFalse(manager.contains(testB.getClass(), testA.getUri(), descriptor(CONTEXT_TWO)));
    }

    @Test
    public void testContainsWithContext() {
        manager.add(testA.getUri(), testA, descriptors(descriptor(CONTEXT_TWO)));
        assertTrue(manager.contains(testA.getClass(), testA.getUri(), descriptor(CONTEXT_TWO)));
        assertFalse(manager.contains(testA.getClass(), testA.getUri(), descriptor(CONTEXT_ONE)));
    }

    @Test
    public void containsWithNullArgumentsReturnsFalse() {
        assertFalse(manager.contains(null, testA.getUri(), descriptor(CONTEXT_TWO)));
        assertFalse(manager.contains(testA.getClass(), null, descriptor(CONTEXT_TWO)));
        assertFalse(manager.contains(testA.getClass(), testA.getUri(), null));
        assertFalse(manager.contains(null, testA.getUri(), descriptor(CONTEXT_TWO)));
        assertFalse(manager.contains(testA.getClass(), null, descriptor(CONTEXT_TWO)));
    }

    @Test
    public void testGetObject() {
        final Descriptor descriptor = descriptor(CONTEXT_TWO);
        manager.add(testA.getUri(), testA, descriptors(descriptor));
        final Object res = manager.get(testA.getClass(), testA.getUri(), descriptor);
        assertEquals(testA, res);
    }

    @Test
    public void getWithWrongContextReturnsNull() {
        final Descriptor descriptor = descriptor(CONTEXT_TWO);
        manager.add(testA.getUri(), testA, descriptors(descriptor));
        assertTrue(manager.contains(testA.getClass(), testA.getUri(), descriptor));
        final OWLClassA res = manager.get(testA.getClass(), testA.getUri(), descriptor(CONTEXT_ONE));
        assertNull(res);
    }

    @Test
    public void getWithNullIdReturnsNull() {
        final Descriptor descriptor = descriptor(CONTEXT_TWO);
        manager.add(testA.getUri(), testA, descriptors(descriptor));
        final Object o = manager.get(OWLClassA.class, null, descriptor);
        assertNull(o);
    }

    @Test
    public void getByNonMatchingClassReturnsNull() {
        final Descriptor descriptor = descriptor(CONTEXT_TWO);
        manager.add(testA.getUri(), testA, descriptors(descriptor));
        manager.add(testB.getUri(), testB, descriptors(descriptor));
        final Object o = manager.get(OWLClassD.class, testA.getUri(), descriptor);
        assertNull(o);
    }

    @Test
    public void getByUnknownIdReturnsNull() {
        final Descriptor descriptor = descriptor(CONTEXT_ONE);
        manager.add(testA.getUri(), testA, descriptors(descriptor));
        manager.add(testB.getUri(), testB, descriptors(descriptor));
        final URI unknownId = URI.create("http://unknownId");
        final Object o = manager.get(OWLClassA.class, unknownId, descriptor);
        assertNull(o);
    }

    @Test
    public void evictAllRemovesAllRecords() {
        final Descriptor descriptor = descriptor(CONTEXT_ONE);
        manager.add(testA.getUri(), testA, descriptors(descriptor));
        addAllToCache(listOfBs, manager);
        manager.evictAll();
        assertFalse(manager.contains(testA.getClass(), testA.getUri(), descriptor));
        for (OWLClassB b : listOfBs.values()) {
            assertFalse(manager.contains(b.getClass(), b.getUri(), descriptor));
        }
    }

    Class<?> evictByClass() {
        final Descriptor descriptorOne = descriptor(CONTEXT_ONE);
        final Descriptor descriptorTwo = descriptor(CONTEXT_TWO);
        manager.add(testA.getUri(), testA, descriptors(descriptorOne));
        manager.add(testB.getUri(), testB, descriptors(descriptorTwo));
        addAllToCache(listOfBs, manager);
        manager.evict(OWLClassB.class);
        assertTrue(manager.contains(OWLClassA.class, testA.getUri(), descriptorOne));
        assertFalse(manager.contains(OWLClassB.class, testB.getUri(), descriptorTwo));
        for (OWLClassB b : listOfBs.values()) {
            assertFalse(manager.contains(b.getClass(), b.getUri(), descriptorTwo));
        }
        return OWLClassB.class;
    }

    @Test
    public void evictByClassThrowsNPXForNullArgument() {
        manager.add(testA.getUri(), testA, descriptors(descriptor(CONTEXT_ONE)));
        addAllToCache(listOfBs, manager);
        assertThrows(NullPointerException.class, () -> manager.evict((Class<?>) null));
    }

    URI evictByContext() {
        final Descriptor descriptorOne = descriptor(CONTEXT_ONE);
        final Descriptor descriptorTwo = descriptor(CONTEXT_TWO);
        manager.add(testA.getUri(), testA, descriptors(descriptorOne));
        manager.add(testB.getUri(), testB, descriptors(descriptorTwo));
        manager.evict(CONTEXT_ONE);
        assertFalse(manager.contains(testA.getClass(), testA.getUri(), descriptorOne));
        assertTrue(manager.contains(testB.getClass(), testB.getUri(), descriptorTwo));
        return CONTEXT_ONE;
    }

    @Test
    public void evictByContextWithNullArgumentEvictsDefaultContext() {
        manager.add(testA.getUri(), testA, descriptors(descriptor(null)));
        manager.evict((URI) null);
        assertFalse(manager.contains(testA.getClass(), testA.getUri(), descriptor(null)));
        assertNull(manager.get(testA.getClass(), testA.getUri(), descriptor(null)));
    }

    @Test
    public void evictByContextRetainsRecordsWhenUnknownContextIsUsed() {
        final Descriptor descriptorOne = descriptor(CONTEXT_ONE);
        final Descriptor descriptorTwo = descriptor(CONTEXT_TWO);
        manager.add(testA.getUri(), testA, descriptors(descriptorOne));
        manager.add(testB.getUri(), testB, descriptors(descriptorTwo));
        manager.evict(URI.create("http://someUnknownContextUri"));
        assertTrue(manager.contains(testA.getClass(), testA.getUri(), descriptorOne));
        assertTrue(manager.contains(testB.getClass(), testB.getUri(), descriptorTwo));
    }

    @Test
    public void evictInferredClassesRemovesInstancesOfInferredClasses() {
        final Descriptor descriptorOne = descriptor(CONTEXT_ONE);
        final Descriptor descriptorTwo = descriptor(CONTEXT_TWO);
        manager.add(testA.getUri(), testA, descriptors(descriptorOne));
        manager.add(testB.getUri(), testB, descriptors(descriptorTwo));
        final Set<Class<?>> inferred = Collections.singleton(testA.getClass());
        manager.setInferredClasses(inferred);
        manager.evictInferredObjects();
        assertFalse(manager.contains(testA.getClass(), testA.getUri(), descriptorOne));
        assertTrue(manager.contains(testB.getClass(), testB.getUri(), descriptorTwo));
    }

    @Test
    public void testEvictByContextClassAndIdentifier() throws Exception {
        final Descriptor descriptorOne = descriptor(CONTEXT_ONE);
        final Descriptor descriptorTwo = descriptor(CONTEXT_TWO);
        manager.add(testA.getUri(), testA, descriptors(descriptorTwo));
        final OWLClassA duplicate = new OWLClassA();
        duplicate.setUri(testA.getUri());
        duplicate.setStringAttribute("Duplicate entity.");
        manager.add(duplicate.getUri(), duplicate, descriptors(descriptorOne));
        assertTrue(manager.contains(testA.getClass(), testA.getUri(), descriptorTwo));
        assertTrue(manager.contains(duplicate.getClass(), duplicate.getUri(), descriptorOne));

        manager.evict(duplicate.getClass(), duplicate.getUri(), CONTEXT_ONE);
        assertFalse(manager.contains(duplicate.getClass(), duplicate.getUri(), descriptorOne));
        assertTrue(manager.contains(testA.getClass(), testA.getUri(), descriptorTwo));
    }

    @Test
    public void evictByNullIdentifierThrowsNPX() {
        manager.add(testA.getUri(), testA, descriptors(descriptor(CONTEXT_ONE)));
        assertThrows(NullPointerException.class, () -> manager.evict(null, null, CONTEXT_ONE));
    }

    private void addAllToCache(Map<URI, OWLClassB> entities, CacheManager manager) {
        final Descriptor descriptor = descriptor(CONTEXT_ONE);
        for (Entry<URI, OWLClassB> e : entities.entrySet()) {
            manager.add(e.getKey(), e.getValue(), descriptors(descriptor));
        }
    }

    @Test
    public void cacheAddWithStringIdentifier() {
        final Descriptor descriptor = descriptor(CONTEXT_ONE);
        manager.add(testM.getKey(), testM, descriptors(descriptor));
        assertTrue(manager.contains(OWLClassM.class, testM.getKey(), descriptor));
    }

    @Test
    public void cacheEvictWithStringIdentifier() {
        final Descriptor descriptor = descriptor(null);
        manager.add(testM.getKey(), testM, descriptors(descriptor));
        assertTrue(manager.contains(OWLClassM.class, testM.getKey(), descriptor));
        manager.evict(OWLClassM.class, testM.getKey(), null);
        assertFalse(manager.contains(OWLClassM.class, testM.getKey(), descriptor));
    }

    @Test
    public void containsReturnsFalseWhenDescriptorsDoNotMatch() {
        final Descriptor descriptorOne = new EntityDescriptor();
        manager.add(testA.getUri(), testA, descriptors(descriptorOne));
        final Descriptor descriptorTwo = new EntityDescriptor();
        descriptorTwo.setLanguage("cs");
        assertTrue(manager.contains(OWLClassA.class, testA.getUri(), descriptorOne));
        assertFalse(manager.contains(OWLClassA.class, testA.getUri(), descriptorTwo));
    }

    @Test
    public void getReturnsNullWhenDescriptorsDoNotMatch() throws Exception {
        final Descriptor descriptorOne = new EntityDescriptor();
        descriptorOne.setLanguage("en");
        manager.add(testA.getUri(), testA, descriptors(descriptorOne));
        final Descriptor descriptorTwo = new EntityDescriptor();
        descriptorTwo.setLanguage("en");
        final Attribute<OWLClassA, String> att = mock(Attribute.class);
        when(att.getJavaField()).thenReturn(OWLClassA.getStrAttField());
        descriptorTwo.setAttributeLanguage(att, "cs");
        assertNotNull(manager.get(OWLClassA.class, testA.getUri(), descriptorOne));
        assertNull(manager.get(OWLClassA.class, testA.getUri(), descriptorTwo));
    }

    @Test
    public void addReplacesInstanceTogetherWithDescriptor() {
        final Descriptor descriptorOne = new EntityDescriptor();
        descriptorOne.setLanguage("en");
        final Descriptor descriptorTwo = new EntityDescriptor();
        descriptorTwo.setLanguage("cs");
        manager.add(testA.getUri(), testA, descriptors(descriptorOne));
        assertTrue(manager.contains(OWLClassA.class, testA.getUri(), descriptorOne));
        assertFalse(manager.contains(OWLClassA.class, testA.getUri(), descriptorTwo));
        manager.add(testA.getUri(), testA, descriptors(descriptorTwo));
        assertFalse(manager.contains(OWLClassA.class, testA.getUri(), descriptorOne));
        assertTrue(manager.contains(OWLClassA.class, testA.getUri(), descriptorTwo));
    }

    @Test
    public void addRemovesOldDescriptorWhenInstanceIsReplacedByAnother() throws Exception {
        final Descriptor descriptorOne = new EntityDescriptor();
        descriptorOne.setLanguage("en");
        final Descriptor descriptorTwo = new EntityDescriptor();
        descriptorTwo.setLanguage("cs");
        manager.add(testA.getUri(), testA, descriptors(descriptorOne));
        assertTrue(manager.contains(OWLClassA.class, testA.getUri(), descriptorOne));
        assertFalse(manager.contains(OWLClassA.class, testA.getUri(), descriptorTwo));
        final OWLClassA newA = new OWLClassA(testA.getUri());
        manager.add(newA.getUri(), newA, descriptors(descriptorTwo));
        assertFalse(manager.contains(OWLClassA.class, testA.getUri(), descriptorOne));
        assertTrue(manager.contains(OWLClassA.class, newA.getUri(), descriptorTwo));
        final Map<?, ?> descriptors = extractDescriptors();
        assertFalse(descriptors.containsKey(testA));
        assertTrue(descriptors.containsKey(newA));
    }

    abstract Map<?, ?> extractDescriptors() throws Exception;

    @Test
    public void evictRemovesInstanceDescriptor() throws Exception {
        final Descriptor descriptorOne = new EntityDescriptor();
        descriptorOne.setLanguage("en");
        manager.add(testA.getUri(), testA, descriptors(descriptorOne));
        assertTrue(extractDescriptors().containsKey(testA));
        manager.evict(OWLClassA.class, testA.getUri(), null);
        assertFalse(extractDescriptors().containsKey(testA));
    }

    @Test
    public void evictByClassRemovesInstanceDescriptors() throws Exception {
        final Descriptor descriptorOne = new EntityDescriptor();
        descriptorOne.setLanguage("en");
        manager.add(testA.getUri(), testA, descriptors(descriptor(CONTEXT_ONE)));
        final OWLClassA anotherA = new OWLClassA(Generators.createIndividualIdentifier());
        manager.add(anotherA.getUri(), anotherA, descriptors(descriptor(null)));
        assertTrue(extractDescriptors().containsKey(testA));
        assertTrue(extractDescriptors().containsKey(anotherA));
        manager.evict(OWLClassA.class);
        assertFalse(extractDescriptors().containsKey(testA));
        assertFalse(extractDescriptors().containsKey(anotherA));
    }

    @Test
    public void evictByContextRemovesInstanceDescriptors() throws Exception {
        final Descriptor descriptorOne = new EntityDescriptor(CONTEXT_ONE);
        manager.add(testA.getUri(), testA, descriptors(descriptorOne));
        final Descriptor descriptorTwo = new EntityDescriptor();
        descriptorTwo.setLanguage("en");
        manager.add(testB.getUri(), testB, descriptors(descriptorTwo));
        assertTrue(manager.contains(OWLClassA.class, testA.getUri(), descriptorOne));
        assertTrue(manager.contains(OWLClassB.class, testB.getUri(), descriptorTwo));
        manager.evict(CONTEXT_ONE);
        assertFalse(manager.contains(OWLClassA.class, testA.getUri(), descriptorOne));
        assertTrue(manager.contains(OWLClassB.class, testB.getUri(), descriptorTwo));
        assertFalse(extractDescriptors().containsKey(testA));
        assertTrue(extractDescriptors().containsKey(testB));
    }

    @Test
    public void cacheSupportsEntitiesOverridingEqualsAndHashCode() {
        final URI uri = Generators.createIndividualIdentifier();
        final Descriptor descriptorOne = new EntityDescriptor(CONTEXT_ONE);
        manager.add(uri, URI.create(uri.toString()), descriptors(descriptorOne));
        manager.add(uri, URI.create(uri.toString()), descriptors(new EntityDescriptor()));
        manager.evict(URI.class, uri, CONTEXT_ONE);
        assertTrue(manager.contains(URI.class, uri, new EntityDescriptor()));
    }

    @Test
    void getLoadStateDescriptorReturnsLoadStateDescriptorAssociatedWithSpecifiedCacheEntry() {
        final Descriptor descriptor = descriptor(CONTEXT_ONE);
        final Descriptors entryDescriptors = descriptors(descriptor);
        manager.add(testM.getKey(), testM, entryDescriptors);
        assertEquals(entryDescriptors.loadStateDescriptor(), manager.getLoadStateDescriptor(testM));
    }
}

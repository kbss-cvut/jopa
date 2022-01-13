/**
 * Copyright (C) 2022 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassD;
import cz.cvut.kbss.jopa.environment.Vocabulary;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.ontodriver.descriptor.ListValueDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.ReferencedListValueDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.SimpleListValueDescriptor;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.*;


public class PendingReferenceRegistryTest {

    private final NamedResource owner = NamedResource.create(Generators.createIndividualIdentifier());
    private final Assertion assertion =
            Assertion.createObjectPropertyAssertion(Generators.createPropertyIdentifier(), false);
    private final Object object = new Object();

    private final PendingReferenceRegistry registry = new PendingReferenceRegistry();

    @Test
    public void addPendingAssertionAddsPendingAssertionToObjectsMap() throws Exception {
        registry.addPendingAssertion(owner, assertion, object, null);

        final Map<Object, Set<PendingAssertion>> assertions = getPendingAssertions();
        assertTrue(assertions.containsKey(object));
        final Set<PendingAssertion> pending = assertions.get(object);
        assertEquals(1, pending.size());
        final PendingAssertion pa = pending.iterator().next();
        assertEquals(owner, pa.getOwner());
        assertEquals(assertion, pa.getAssertion());
        assertNull(pa.getContext());
    }

    @Test
    public void addPendingAssertionAddsPendingAssertionToObjectsMapWithContext() throws Exception {
        final URI context = Generators.createIndividualIdentifier();
        registry.addPendingAssertion(owner, assertion, object, context);

        final Map<Object, Set<PendingAssertion>> assertions = getPendingAssertions();
        assertTrue(assertions.containsKey(object));
        final Set<PendingAssertion> pending = assertions.get(object);
        assertEquals(1, pending.size());
        final PendingAssertion pa = pending.iterator().next();
        assertEquals(owner, pa.getOwner());
        assertEquals(assertion, pa.getAssertion());
        assertEquals(context, pa.getContext());
    }

    @SuppressWarnings("unchecked")
    private Map<Object, Set<PendingAssertion>> getPendingAssertions() throws Exception {
        final Field paField = PendingReferenceRegistry.class.getDeclaredField("pendingAssertions");
        paField.setAccessible(true);
        return (Map<Object, Set<PendingAssertion>>) paField.get(registry);
    }

    @Test
    public void removeAndGetRemovesPendingAssertionsForObjectAndReturnsThem() throws Exception {
        registry.addPendingAssertion(owner, assertion, object, null);
        final Map<Object, Set<PendingAssertion>> assertions = getPendingAssertions();
        final Set<PendingAssertion> pending = assertions.get(object);
        assertFalse(pending.isEmpty());

        final Set<PendingAssertion> result =
                registry.removeAndGetPendingAssertionsWith(object);
        assertEquals(pending, result);
        assertFalse(getPendingAssertions().containsKey(object));
    }

    @Test
    public void removePendingAssertionsRemovesAssertionsRegardingSpecifiedSubject() throws Exception {
        registry.addPendingAssertion(owner, assertion, object, null);
        registry.addPendingAssertion(NamedResource.create(Generators.createIndividualIdentifier()), assertion, object,
                null);
        final Object anotherObject = new Object();
        registry.addPendingAssertion(owner, assertion, anotherObject, null);
        registry.removePendingReferences(owner);
        final Map<Object, Set<PendingAssertion>> result = getPendingAssertions();
        assertEquals(1, result.get(object).size());
        assertFalse(result.containsKey(anotherObject));
        assertTrue(registry.getPendingResources().contains(object));
        assertFalse(registry.getPendingResources().contains(anotherObject));
    }

    @Test
    public void removePendingAssertionsRemovesAssertionsWithSubjectAndAssertion() throws Exception {
        registry.addPendingAssertion(owner, assertion, object, null);
        registry.addPendingAssertion(NamedResource.create(Generators.createIndividualIdentifier()), assertion, object,
                null);
        registry.removePendingReferences(owner, assertion);
        final Map<Object, Set<PendingAssertion>> result = getPendingAssertions();
        assertEquals(1, result.get(object).size());
        final PendingAssertion pa = result.get(object).iterator().next();
        assertNotEquals(owner, pa.getOwner());
    }

    @Test
    public void addPendingListReferenceRegistersListAndPendingItem() throws Exception {
        final SimpleListValueDescriptor desc = new SimpleListValueDescriptor(owner, assertion, assertion);
        final List<OWLClassA> list = Generators.generateInstances(10);
        final OWLClassA item = list.get(0);
        registry.addPendingListReference(item, desc, list);
        assertTrue(registry.getPendingResources().contains(item));
        assertEquals(1, getPendingItemCount(desc));
    }

    @SuppressWarnings("unchecked")
    private int getPendingItemCount(ListValueDescriptor descriptor) throws Exception {
        final Field mapField = PendingReferenceRegistry.class.getDeclaredField("pendingListItems");
        mapField.setAccessible(true);
        final Map<ListValueDescriptor, Integer> map = (Map<ListValueDescriptor, Integer>) mapField.get(registry);
        return map.getOrDefault(descriptor, 0);
    }

    @Test
    public void addPendingListReferenceIncrementsCounterIfAnotherItemInListIsAlreadyPending() throws Exception {
        final ListValueDescriptor desc = new ReferencedListValueDescriptor(owner, assertion, assertion, assertion);
        final List<OWLClassA> list = Generators.generateInstances(10);
        for (int i = 0; i < list.size(); i++) {
            final OWLClassA item = list.get(i);
            registry.addPendingListReference(item, desc, list);
            assertTrue(registry.getPendingResources().contains(item));
            assertEquals(i + 1, getPendingItemCount(desc));
        }
    }

    @Test
    public void removeAndGetPendingReferencesToListGetsDescriptorsOfListsToPersist() {
        final SimpleListValueDescriptor desc = new SimpleListValueDescriptor(owner, assertion, assertion);
        final List<OWLClassA> list = Generators.generateInstances(10);
        final OWLClassA item = list.get(0);
        registry.addPendingListReference(item, desc, list);
        assertTrue(registry.getPendingResources().contains(item));
        final Set<PendingReferenceRegistry.PendingListReference> refs =
                registry.removeAndGetPendingListReferencesWith(item);
        assertEquals(1, refs.size());
        final PendingReferenceRegistry.PendingListReference ref = refs.iterator().next();
        assertEquals(desc, ref.getDescriptor());
        assertEquals(list, ref.getValues());
        assertFalse(registry.getPendingResources().contains(item));
    }

    @Test
    public void removeAndGetPendingReferencesToListDoesNotReturnDescriptorWhereOtherPendingItemsExist() throws
            Exception {
        final SimpleListValueDescriptor desc = new SimpleListValueDescriptor(owner, assertion, assertion);
        final List<OWLClassA> list = Generators.generateInstances(10);
        final OWLClassA item = list.get(0);
        final OWLClassA retained = list.get(1);
        registry.addPendingListReference(item, desc, list);
        registry.addPendingListReference(retained, desc, list);
        assertTrue(registry.getPendingResources().contains(item));
        assertTrue(registry.getPendingResources().contains(retained));
        final Set<PendingReferenceRegistry.PendingListReference> refs =
                registry.removeAndGetPendingListReferencesWith(item);
        assertTrue(refs.isEmpty());
        assertFalse(registry.getPendingResources().contains(item));
        assertTrue(registry.getPendingResources().contains(retained));
        assertEquals(1, getPendingItemCount(desc));
    }

    @Test
    public void removeAndGetPendingListReferencesReturnsEmptyCollectionWhenNoPendingListsExistForItem() {
        final Set<PendingReferenceRegistry.PendingListReference> result =
                registry.removeAndGetPendingListReferencesWith(new OWLClassA());
        assertNotNull(result);
        assertTrue(result.isEmpty());
    }

    @Test
    public void hasPendingResourcesReturnsTrueForPendingAssertions() {
        assertFalse(registry.hasPendingResources());
        registry.addPendingAssertion(owner, assertion, object, null);
        assertTrue(registry.hasPendingResources());
    }

    @Test
    public void hasPendingResourcesReturnsTrueForPendingListReferences() {
        final SimpleListValueDescriptor desc = new SimpleListValueDescriptor(owner, assertion, assertion);
        final List<OWLClassA> list = Generators.generateInstances(10);
        registry.addPendingListReference(list.get(0), desc, list);
        assertTrue(registry.hasPendingResources());
    }

    @Test
    public void removePendingReferenceRemovesListReferenceWhenOwnerIsRemoved() throws Exception {
        final SimpleListValueDescriptor desc = new SimpleListValueDescriptor(owner, assertion, assertion);
        final List<OWLClassA> list = Generators.generateInstances(10);
        registry.addPendingListReference(list.get(0), desc, list);
        registry.addPendingListReference(list.get(2), desc, list);
        assertFalse(registry.getPendingResources().isEmpty());
        registry.removePendingReferences(desc.getListOwner());
        assertTrue(registry.getPendingResources().isEmpty());
        assertEquals(0, getPendingItemCount(desc));
    }

    @Test
    public void removePendingReferenceKeepsReferencesToListsWithDifferentOwner() throws Exception {
        final SimpleListValueDescriptor desc = new SimpleListValueDescriptor(owner, assertion, assertion);
        final List<OWLClassA> list = Generators.generateInstances(10);
        final OWLClassA common = list.get(0);
        registry.addPendingListReference(common, desc, list);
        final NamedResource other = NamedResource.create(Generators.createIndividualIdentifier());
        final ReferencedListValueDescriptor descTwo =
                new ReferencedListValueDescriptor(other, assertion, assertion, assertion);
        final List<OWLClassA> otherList = Generators.generateInstances(10);
        otherList.add(common);
        registry.addPendingListReference(otherList.get(0), descTwo, otherList);
        registry.addPendingListReference(common, descTwo, otherList);  // This is the common one

        registry.removePendingReferences(owner);
        final Set<Object> pending = registry.getPendingResources();
        assertTrue(pending.contains(list.get(0)));
        assertEquals(0, getPendingItemCount(desc));
        assertEquals(2, getPendingItemCount(descTwo));
    }

    @Test
    public void removePendingAssertionsRemovesPendingListReference() throws Exception {
        final SimpleListValueDescriptor desc = new SimpleListValueDescriptor(owner, assertion, assertion);
        final List<OWLClassA> list = Generators.generateInstances(10);
        list.forEach(a -> registry.addPendingListReference(a, desc, list));

        registry.removePendingReferences(owner, assertion);
        assertTrue(registry.getPendingResources().isEmpty());
        assertEquals(0, getPendingItemCount(desc));
    }

    /**
     * Based on a reported bug.
     */
    @Test
    void removePendingAssertionWorksForEntityWithOverriddenEqualsAndHashCodeWhenItWasInsertedWithoutId() {
        final OWLClassD referer = new OWLClassD(Generators.createIndividualIdentifier());
        final ObjectWithEquals object = new ObjectWithEquals();
        registry.addPendingAssertion(NamedResource.create(referer.getUri()),
                Assertion.createObjectPropertyAssertion(URI.create(
                        Vocabulary.P_HAS_A), false), object, null);
        assertTrue(registry.hasPendingResources());
        object.uri = Generators.createIndividualIdentifier();
        registry.removeAndGetPendingAssertionsWith(object);
        assertFalse(registry.hasPendingResources());
    }

    private static class ObjectWithEquals {
        @Id
        private URI uri;

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (!(o instanceof ObjectWithEquals)) return false;
            ObjectWithEquals that = (ObjectWithEquals) o;
            return Objects.equals(uri, that.uri);
        }

        @Override
        public int hashCode() {
            return Objects.hash(uri);
        }
    }
}

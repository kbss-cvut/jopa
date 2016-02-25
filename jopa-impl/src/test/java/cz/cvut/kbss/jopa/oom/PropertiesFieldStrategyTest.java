/**
 * Copyright (C) 2016 Czech Technical University in Prague
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

import cz.cvut.kbss.jopa.environment.OWLClassB;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.environment.utils.TestEnvironmentUtils;
import cz.cvut.kbss.jopa.exceptions.InvalidAssertionIdentifierException;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.net.URI;
import java.util.*;

import static org.junit.Assert.*;
import static org.mockito.Mockito.when;

public class PropertiesFieldStrategyTest {

    private static final URI PK = URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#entityB");

    @Mock
    private EntityMappingHelper mapperMock;

    private PropertiesFieldStrategy<OWLClassB> strategy;
    private AxiomValueGatherer gatherer;

    private OWLClassB entityB;

    @Before
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        MetamodelMocks mocks = new MetamodelMocks();

        this.entityB = new OWLClassB();
        entityB.setUri(PK);
        entityB.setStringAttribute("someString");
        final Descriptor descriptor = new EntityDescriptor();

        this.strategy = new PropertiesFieldStrategy<>(mocks.forOwlClassB().entityType(),
                mocks.forOwlClassB().propertiesSpec(), descriptor, mapperMock);
        this.gatherer = new AxiomValueGatherer(NamedResource.create(PK), null);
        gatherer.addValue(Assertion.createClassAssertion(false), new Value<>(PK), null);
    }

    @Test
    public void extractsValuesForPersist() throws Exception {
        final int propCount = 5;
        entityB.setProperties(TestEnvironmentUtils.generateProperties(propCount, propCount));
        when(mapperMock.getOriginalInstance(entityB)).thenReturn(null);

        strategy.buildAxiomValuesFromInstance(entityB, gatherer);

        final Map<Assertion, Set<Value<?>>> res = OOMTestUtils.getPropertiesToAdd(gatherer);
        assertTrue(collectionsAreEqual(entityB.getProperties(), res));
        assertNull(OOMTestUtils.getPropertiesToRemove(gatherer));
    }

    @Test
    public void extractsNothingWhenThereAreNoPropertiesForPersist() throws Exception {
        when(mapperMock.getOriginalInstance(entityB)).thenReturn(null);

        strategy.buildAxiomValuesFromInstance(entityB, gatherer);

        assertNull(OOMTestUtils.getPropertiesToAdd(gatherer));
        assertNull(OOMTestUtils.getPropertiesToRemove(gatherer));
    }

    @Test
    public void extractsNothingWhenThereAreNotPropertiesToAddAndNoneInOriginal() throws Exception {
        when(mapperMock.getOriginalInstance(entityB)).thenReturn(createOriginal());

        strategy.buildAxiomValuesFromInstance(entityB, gatherer);

        assertNull(OOMTestUtils.getPropertiesToAdd(gatherer));
        assertNull(OOMTestUtils.getPropertiesToRemove(gatherer));
    }

    private OWLClassB createOriginal() {
        final OWLClassB original = new OWLClassB();
        original.setUri(PK);
        original.setStringAttribute(entityB.getStringAttribute());
        if (entityB.getProperties() != null) {
            final Map<String, Set<String>> copy = new HashMap<>();
            for (Map.Entry<String, Set<String>> e : entityB.getProperties().entrySet()) {
                copy.put(e.getKey(), new HashSet<>(e.getValue()));
            }
            original.setProperties(copy);
        }
        return original;
    }

    @Test(expected = InvalidAssertionIdentifierException.class)
    public void throwsExceptionWhenPropertyIsNotAValidURI() throws Exception {
        entityB.setProperties(TestEnvironmentUtils.generateProperties(1, 1));
        entityB.getProperties().put("blabla^", Collections.<String>emptySet());
        when(mapperMock.getOriginalInstance(entityB)).thenReturn(null);

        strategy.buildAxiomValuesFromInstance(entityB, gatherer);
    }

    @Test
    public void removesAllPropertiesWhenSetToNull() throws Exception {
        final Map<String, Set<String>> properties = TestEnvironmentUtils.generateProperties(5, 5);
        entityB.setProperties(properties);
        when(mapperMock.getOriginalInstance(entityB)).thenReturn(createOriginal());
        entityB.setProperties(null);

        strategy.buildAxiomValuesFromInstance(entityB, gatherer);
        final Map<Assertion, Set<Value<?>>> result = OOMTestUtils.getPropertiesToRemove(gatherer);
        assertTrue(collectionsAreEqual(properties, result));
        assertNull(OOMTestUtils.getPropertiesToAdd(gatherer));
    }

    @Test
    public void removesAllPropertiesWhenCleared() throws Exception {
        final Map<String, Set<String>> properties = TestEnvironmentUtils.generateProperties(5, 5);
        entityB.setProperties(properties);
        when(mapperMock.getOriginalInstance(entityB)).thenReturn(createOriginal());
        entityB.setProperties(Collections.<String, Set<String>>emptyMap());

        strategy.buildAxiomValuesFromInstance(entityB, gatherer);
        final Map<Assertion, Set<Value<?>>> result = OOMTestUtils.getPropertiesToRemove(gatherer);
        assertTrue(collectionsAreEqual(properties, result));
        assertNull(OOMTestUtils.getPropertiesToAdd(gatherer));
    }

    @Test
    public void removesSeveralWholeProperties() throws Exception {
        entityB.setProperties(TestEnvironmentUtils.generateProperties(5, 5));
        when(mapperMock.getOriginalInstance(entityB)).thenReturn(createOriginal());
        // Remove one property with values
        final String property = entityB.getProperties().keySet().iterator().next();
        final Set<String> values = entityB.getProperties().get(property);
        entityB.getProperties().remove(property);

        strategy.buildAxiomValuesFromInstance(entityB, gatherer);
        final Map<Assertion, Set<Value<?>>> result = OOMTestUtils.getPropertiesToRemove(gatherer);
        assertTrue(collectionsAreEqual(Collections.singletonMap(property, values), result));
        assertNull(OOMTestUtils.getPropertiesToAdd(gatherer));
    }

    @Test
    public void removesValuesOfSomeProperties() throws Exception {
        entityB.setProperties(TestEnvironmentUtils.generateProperties(5, 5));
        when(mapperMock.getOriginalInstance(entityB)).thenReturn(createOriginal());
        final Map<String, Set<String>> toRemove = new HashMap<>();
        int propIndex = 0;
        for (Map.Entry<String, Set<String>> e : entityB.getProperties().entrySet()) {
            if (propIndex++ % 2 != 0) {
                toRemove.put(e.getKey(), new HashSet<>());
                int valueIndex = 0;
                final Iterator<String> it = e.getValue().iterator();
                while (it.hasNext()) {
                    final String val = it.next();
                    if (valueIndex++ % 2 != 0) {
                        toRemove.get(e.getKey()).add(val);
                        it.remove();
                    }
                }
            }
        }

        strategy.buildAxiomValuesFromInstance(entityB, gatherer);
        final Map<Assertion, Set<Value<?>>> result = OOMTestUtils.getPropertiesToRemove(gatherer);
        assertTrue(collectionsAreEqual(toRemove, result));
        assertNull(OOMTestUtils.getPropertiesToAdd(gatherer));
    }

    @Test
    public void addsPropertyWithValues() throws Exception {
        entityB.setProperties(TestEnvironmentUtils.generateProperties(3, 3));
        when(mapperMock.getOriginalInstance(entityB)).thenReturn(createOriginal());
        final Map<String, Set<String>> added = new HashMap<>();
        added.put("http://krizik.felk.cvut.cz/ontologies/jopa#added",
                new HashSet<>(Arrays.asList("one", "two", "http://three")));
        entityB.getProperties().putAll(added);

        strategy.buildAxiomValuesFromInstance(entityB, gatherer);

        final Map<Assertion, Set<Value<?>>> result = OOMTestUtils.getPropertiesToAdd(gatherer);
        assertTrue(collectionsAreEqual(added, result));
        assertNull(OOMTestUtils.getPropertiesToRemove(gatherer));
    }

    @Test
    public void addsValuesForExistingProperty() throws Exception {
        entityB.setProperties(TestEnvironmentUtils.generateProperties(3, 3));
        when(mapperMock.getOriginalInstance(entityB)).thenReturn(createOriginal());
        final Map<String, Set<String>> added = new HashMap<>();
        final String property = entityB.getProperties().keySet().iterator().next();
        final Set<String> newValues = new HashSet<>(Arrays.asList("one", "seven", "http://krizik.felk.cvut.cz/valueS"));
        added.put(property, newValues);
        entityB.getProperties().get(property).addAll(newValues);

        strategy.buildAxiomValuesFromInstance(entityB, gatherer);

        final Map<Assertion, Set<Value<?>>> result = OOMTestUtils.getPropertiesToAdd(gatherer);
        assertTrue(collectionsAreEqual(added, result));
        assertNull(OOMTestUtils.getPropertiesToRemove(gatherer));
    }

    // This is a mix of the previous tests
    @Test
    public void addsAndRemovesSomePropertiesWithValuesAndPropertyValues() throws Exception {
        entityB.setProperties(TestEnvironmentUtils.generateProperties(3, 3));
        when(mapperMock.getOriginalInstance(entityB)).thenReturn(createOriginal());
        final Map<String, Set<String>> removed = prepareForRemove();
        final Map<String, Set<String>> added = prepareForAdd();

        strategy.buildAxiomValuesFromInstance(entityB, gatherer);

        final Map<Assertion, Set<Value<?>>> resultAdded = OOMTestUtils.getPropertiesToAdd(gatherer);
        final Map<Assertion, Set<Value<?>>> resultRemoved = OOMTestUtils.getPropertiesToRemove(gatherer);
        assertTrue(collectionsAreEqual(added, resultAdded));
        assertTrue(collectionsAreEqual(removed, resultRemoved));
    }

    private Map<String, Set<String>> prepareForAdd() {
        final Map<String, Set<String>> added = new HashMap<>();
        final String property = entityB.getProperties().keySet().iterator().next();
        final Set<String> newValues = new HashSet<>(Arrays.asList("one", "seven", "http://krizik.felk.cvut.cz/valueS"));
        added.put(property, newValues);
        entityB.getProperties().get(property).addAll(newValues);
        final Map<String, Set<String>> newProperty = Collections
                .<String, Set<String>>singletonMap("http://krizik.felk.cvut.cz/ontologies/jopa#newProperty",
                        new HashSet<>(Arrays.asList("blablaOne", "blablaTwo", "http://blabla.org")));
        added.putAll(newProperty);
        entityB.getProperties().putAll(newProperty);
        return added;
    }

    private Map<String, Set<String>> prepareForRemove() {
        final Map<String, Set<String>> removed = new HashMap<>();
        String propertyToUpdate = null;
        String propertyToRemove = null;
        Iterator<String> it = entityB.getProperties().keySet().iterator();
        while (it.hasNext()) {
            propertyToUpdate = propertyToRemove;
            propertyToRemove = it.next();
        }
        removed.put(propertyToRemove, entityB.getProperties().get(propertyToRemove));
        entityB.getProperties().remove(propertyToRemove);
        final Set<String> removedValues = new HashSet<>();
        it = entityB.getProperties().get(propertyToUpdate).iterator();
        int ind = 0;
        while (it.hasNext()) {
            String val = it.next();
            if (ind++ % 2 != 0) {
                it.remove();
                removedValues.add(val);
            }
        }
        removed.put(propertyToUpdate, removedValues);
        return removed;
    }

    private static boolean collectionsAreEqual(Map<String, Set<String>> expected,
                                               Map<Assertion, Set<Value<?>>> actual) {
        if (expected.size() != actual.size()) {
            return false;
        }
        for (Map.Entry<String, Set<String>> entry : expected.entrySet()) {
            final Set<Value<?>> values = actual
                    .get(Assertion.createPropertyAssertion(URI.create(entry.getKey()), false));
            if (values == null) {
                return false;
            }
            assertEquals(entry.getValue().size(), values.size());
            for (Value<?> val : values) {
                if (!entry.getValue().contains(val.stringValue())) {
                    return false;
                }
            }
        }
        return true;
    }
}
/**
 * Copyright (C) 2020 Czech Technical University in Prague
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
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.environment.utils.TestEnvironmentUtils;
import cz.cvut.kbss.jopa.exceptions.InvalidAssertionIdentifierException;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.ontodriver.model.*;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.net.URI;
import java.util.*;
import java.util.stream.Collectors;

import static org.junit.Assert.*;
import static org.mockito.Mockito.when;

public class StringPropertiesFieldStrategyTest {

    private static final URI PK = Generators.createIndividualIdentifier();

    @Mock
    private EntityMappingHelper mapperMock;

    private PropertiesFieldStrategy<OWLClassB> strategy;
    private AxiomValueGatherer gatherer;

    private OWLClassB entity;

    @Before
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        MetamodelMocks mocks = new MetamodelMocks();

        this.entity = new OWLClassB();
        entity.setUri(PK);
        entity.setStringAttribute("someString");
        final Descriptor descriptor = new EntityDescriptor();

        this.strategy = new PropertiesFieldStrategy<>(mocks.forOwlClassB().entityType(),
                mocks.forOwlClassB().propertiesSpec(), descriptor, mapperMock);
        this.gatherer = new AxiomValueGatherer(NamedResource.create(PK), null);
        gatherer.addValue(Assertion.createClassAssertion(false), new Value<>(PK), null);
    }

    @Test
    public void extractsValuesForPersist() throws Exception {
        final int propCount = 5;
        entity.setProperties(Generators.generateStringProperties(propCount, propCount));
        when(mapperMock.getOriginalInstance(entity)).thenReturn(null);

        strategy.buildAxiomValuesFromInstance(entity, gatherer);

        final Map<Assertion, Set<Value<?>>> res = OOMTestUtils.getPropertiesToAdd(gatherer);
        assertTrue(TestEnvironmentUtils.assertionsCorrespondToProperties(entity.getProperties(), res));
        assertNull(OOMTestUtils.getPropertiesToRemove(gatherer));
    }

    @Test
    public void extractsNothingWhenThereAreNoPropertiesForPersist() throws Exception {
        when(mapperMock.getOriginalInstance(entity)).thenReturn(null);

        strategy.buildAxiomValuesFromInstance(entity, gatherer);

        assertNull(OOMTestUtils.getPropertiesToAdd(gatherer));
        assertNull(OOMTestUtils.getPropertiesToRemove(gatherer));
    }

    @Test
    public void extractsNothingWhenThereAreNotPropertiesToAddAndNoneInOriginal() throws Exception {
        when(mapperMock.getOriginalInstance(entity)).thenReturn(createOriginal());

        strategy.buildAxiomValuesFromInstance(entity, gatherer);

        assertNull(OOMTestUtils.getPropertiesToAdd(gatherer));
        assertNull(OOMTestUtils.getPropertiesToRemove(gatherer));
    }

    @Test
    public void extractFieldValueSkipsNullPropertyIdentifier() throws Exception {
        final int propCount = 5;
        final Map<String, Set<String>> props = Generators.generateStringProperties(propCount, propCount);
        entity.setProperties(new HashMap<>(props));
        entity.getProperties().put(null, Collections.singleton("Test"));
        when(mapperMock.getOriginalInstance(entity)).thenReturn(null);

        strategy.buildAxiomValuesFromInstance(entity, gatherer);

        final Map<Assertion, Set<Value<?>>> res = OOMTestUtils.getPropertiesToAdd(gatherer);
        assertTrue(TestEnvironmentUtils.assertionsCorrespondToProperties(props, res));
        assertNull(OOMTestUtils.getPropertiesToRemove(gatherer));
    }

    @Test
    public void extractFieldValueSkipsNullSetOfPropertyValues() throws Exception {
        final Map<String, Set<String>> props = Generators.generateStringProperties();
        entity.setProperties(new HashMap<>(props));
        entity.getProperties().put("http://krizik.felk.cvut.cz/ontologies/nullProperty", null);
        when(mapperMock.getOriginalInstance(entity)).thenReturn(null);

        strategy.buildAxiomValuesFromInstance(entity, gatherer);

        final Map<Assertion, Set<Value<?>>> res = OOMTestUtils.getPropertiesToAdd(gatherer);
        assertTrue(TestEnvironmentUtils.assertionsCorrespondToProperties(props, res));
        assertNull(OOMTestUtils.getPropertiesToRemove(gatherer));
    }

    @Test
    public void extractFieldValueSkipsNullPropertyValue() throws Exception {
        final Map<String, Set<String>> props = Generators.generateStringProperties();
        entity.setProperties(new HashMap<>());
        props.forEach((key, val) -> entity.getProperties().put(key, new HashSet<>(val)));
        final String property = entity.getProperties().keySet().iterator().next();
        entity.getProperties().get(property).add(null);
        when(mapperMock.getOriginalInstance(entity)).thenReturn(null);

        strategy.buildAxiomValuesFromInstance(entity, gatherer);

        final Map<Assertion, Set<Value<?>>> res = OOMTestUtils.getPropertiesToAdd(gatherer);
        assertTrue(TestEnvironmentUtils.assertionsCorrespondToProperties(props, res));
        assertNull(OOMTestUtils.getPropertiesToRemove(gatherer));
    }

    private OWLClassB createOriginal() {
        final OWLClassB original = new OWLClassB();
        original.setUri(PK);
        original.setStringAttribute(entity.getStringAttribute());
        if (entity.getProperties() != null) {
            final Map<String, Set<String>> copy = new HashMap<>();
            for (Map.Entry<String, Set<String>> e : entity.getProperties().entrySet()) {
                copy.put(e.getKey(), new HashSet<>(e.getValue()));
            }
            original.setProperties(copy);
        }
        return original;
    }

    @Test(expected = InvalidAssertionIdentifierException.class)
    public void throwsExceptionWhenPropertyIsNotAValidURI() throws Exception {
        entity.setProperties(Generators.generateStringProperties(1, 1));
        entity.getProperties().put("blabla^", Collections.emptySet());
        when(mapperMock.getOriginalInstance(entity)).thenReturn(null);

        strategy.buildAxiomValuesFromInstance(entity, gatherer);
    }

    @Test
    public void removesAllPropertiesWhenSetToNull() throws Exception {
        final Map<String, Set<String>> properties = Generators.generateStringProperties();
        entity.setProperties(properties);
        when(mapperMock.getOriginalInstance(entity)).thenReturn(createOriginal());
        entity.setProperties(null);

        strategy.buildAxiomValuesFromInstance(entity, gatherer);
        final Map<Assertion, Set<Value<?>>> result = OOMTestUtils.getPropertiesToRemove(gatherer);
        assertTrue(TestEnvironmentUtils.assertionsCorrespondToProperties(properties, result));
        assertNull(OOMTestUtils.getPropertiesToAdd(gatherer));
    }

    @Test
    public void removesAllPropertiesWhenCleared() throws Exception {
        final Map<String, Set<String>> properties = Generators.generateStringProperties();
        entity.setProperties(properties);
        when(mapperMock.getOriginalInstance(entity)).thenReturn(createOriginal());
        entity.setProperties(Collections.emptyMap());

        strategy.buildAxiomValuesFromInstance(entity, gatherer);
        final Map<Assertion, Set<Value<?>>> result = OOMTestUtils.getPropertiesToRemove(gatherer);
        assertTrue(TestEnvironmentUtils.assertionsCorrespondToProperties(properties, result));
        assertNull(OOMTestUtils.getPropertiesToAdd(gatherer));
    }

    @Test
    public void removesSeveralWholeProperties() throws Exception {
        entity.setProperties(Generators.generateStringProperties(5, 5));
        when(mapperMock.getOriginalInstance(entity)).thenReturn(createOriginal());
        // Remove one property with values
        final String property = entity.getProperties().keySet().iterator().next();
        final Set<String> values = entity.getProperties().get(property);
        entity.getProperties().remove(property);

        strategy.buildAxiomValuesFromInstance(entity, gatherer);
        final Map<Assertion, Set<Value<?>>> result = OOMTestUtils.getPropertiesToRemove(gatherer);
        assertTrue(TestEnvironmentUtils
                .assertionsCorrespondToProperties(Collections.singletonMap(property, values), result));
        assertNull(OOMTestUtils.getPropertiesToAdd(gatherer));
    }

    @Test
    public void removesValuesOfSomeProperties() throws Exception {
        entity.setProperties(Generators.generateStringProperties());
        when(mapperMock.getOriginalInstance(entity)).thenReturn(createOriginal());
        final Map<String, Set<String>> toRemove = new HashMap<>();
        int propIndex = 0;
        for (Map.Entry<String, Set<String>> e : entity.getProperties().entrySet()) {
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

        strategy.buildAxiomValuesFromInstance(entity, gatherer);
        final Map<Assertion, Set<Value<?>>> result = OOMTestUtils.getPropertiesToRemove(gatherer);
        assertTrue(TestEnvironmentUtils.assertionsCorrespondToProperties(toRemove, result));
        assertNull(OOMTestUtils.getPropertiesToAdd(gatherer));
    }

    @Test
    public void addsPropertyWithValues() throws Exception {
        entity.setProperties(Generators.generateStringProperties(3, 3));
        when(mapperMock.getOriginalInstance(entity)).thenReturn(createOriginal());
        final Map<String, Set<String>> added = new HashMap<>();
        added.put("http://krizik.felk.cvut.cz/ontologies/jopa#added",
                new HashSet<>(Arrays.asList("one", "two", "http://three")));
        entity.getProperties().putAll(added);

        strategy.buildAxiomValuesFromInstance(entity, gatherer);

        final Map<Assertion, Set<Value<?>>> result = OOMTestUtils.getPropertiesToAdd(gatherer);
        assertTrue(TestEnvironmentUtils.assertionsCorrespondToProperties(added, result));
        assertNull(OOMTestUtils.getPropertiesToRemove(gatherer));
    }

    @Test
    public void addsValuesForExistingProperty() throws Exception {
        entity.setProperties(Generators.generateStringProperties(3, 3));
        when(mapperMock.getOriginalInstance(entity)).thenReturn(createOriginal());
        final Map<String, Set<String>> added = new HashMap<>();
        final String property = entity.getProperties().keySet().iterator().next();
        final Set<String> newValues = new HashSet<>(Arrays.asList("one", "seven", "http://krizik.felk.cvut.cz/valueS"));
        added.put(property, newValues);
        entity.getProperties().get(property).addAll(newValues);

        strategy.buildAxiomValuesFromInstance(entity, gatherer);

        final Map<Assertion, Set<Value<?>>> result = OOMTestUtils.getPropertiesToAdd(gatherer);
        assertTrue(TestEnvironmentUtils.assertionsCorrespondToProperties(added, result));
        assertNull(OOMTestUtils.getPropertiesToRemove(gatherer));
    }

    // This is a mix of the previous tests
    @Test
    public void addsAndRemovesSomePropertiesWithValuesAndPropertyValues() throws Exception {
        entity.setProperties(Generators.generateStringProperties(3, 3));
        when(mapperMock.getOriginalInstance(entity)).thenReturn(createOriginal());
        final Map<String, Set<String>> removed = prepareForRemove();
        final Map<String, Set<String>> added = prepareForAdd();

        strategy.buildAxiomValuesFromInstance(entity, gatherer);

        final Map<Assertion, Set<Value<?>>> resultAdded = OOMTestUtils.getPropertiesToAdd(gatherer);
        final Map<Assertion, Set<Value<?>>> resultRemoved = OOMTestUtils.getPropertiesToRemove(gatherer);
        assertTrue(TestEnvironmentUtils.assertionsCorrespondToProperties(added, resultAdded));
        assertTrue(TestEnvironmentUtils.assertionsCorrespondToProperties(removed, resultRemoved));
    }

    @Test
    public void extractsAllPropertiesForAddWhenThereWereNoneInOriginal() throws Exception {
        when(mapperMock.getOriginalInstance(entity)).thenReturn(createOriginal());
        assertNull(mapperMock.getOriginalInstance(entity).getProperties());
        entity.setProperties(Generators.generateStringProperties(3, 3));
        strategy.buildAxiomValuesFromInstance(entity, gatherer);
        final Map<Assertion, Set<Value<?>>> resultAdded = OOMTestUtils.getPropertiesToAdd(gatherer);
        assertTrue(TestEnvironmentUtils.assertionsCorrespondToProperties(entity.getProperties(), resultAdded));
    }

    private Map<String, Set<String>> prepareForAdd() {
        final Map<String, Set<String>> added = new HashMap<>();
        final String property = entity.getProperties().keySet().iterator().next();
        final Set<String> newValues = new HashSet<>(Arrays.asList("one", "seven", "http://krizik.felk.cvut.cz/valueS"));
        added.put(property, newValues);
        entity.getProperties().get(property).addAll(newValues);
        final Map<String, Set<String>> newProperty = Collections
                .singletonMap("http://krizik.felk.cvut.cz/ontologies/jopa#newProperty",
                        new HashSet<>(Arrays.asList("blablaOne", "blablaTwo", "http://blabla.org")));
        added.putAll(newProperty);
        entity.getProperties().putAll(newProperty);
        return added;
    }

    private Map<String, Set<String>> prepareForRemove() {
        final Map<String, Set<String>> removed = new HashMap<>();
        String propertyToUpdate = null;
        String propertyToRemove = null;
        Iterator<String> it = entity.getProperties().keySet().iterator();
        while (it.hasNext()) {
            propertyToUpdate = propertyToRemove;
            propertyToRemove = it.next();
        }
        removed.put(propertyToRemove, entity.getProperties().get(propertyToRemove));
        entity.getProperties().remove(propertyToRemove);
        final Set<String> removedValues = new HashSet<>();
        it = entity.getProperties().get(propertyToUpdate).iterator();
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

    @Test
    public void buildsInstanceFieldFroAxiomValues() throws Exception {
        final Map<String, Set<String>> properties = Generators.generateStringProperties();
        final Collection<Axiom<?>> axioms = createAxiomsForProperties(properties);
        axioms.forEach(ax -> strategy.addValueFromAxiom(ax));
        strategy.buildInstanceFieldValue(entity);
        assertEquals(properties, entity.getProperties());
    }

    private Collection<Axiom<?>> createAxiomsForProperties(Map<String, Set<String>> data) {
        final NamedResource subject = NamedResource.create(PK);
        final Collection<Axiom<?>> axioms = new ArrayList<>();
        for (Map.Entry<String, Set<String>> e : data.entrySet()) {
            axioms.addAll(e.getValue().stream()
                           .map(val -> new AxiomImpl<>(subject,
                                   Assertion.createPropertyAssertion(URI.create(e.getKey()), false), new Value<>(val)))
                           .collect(Collectors.toList()));
        }
        return axioms;
    }
}
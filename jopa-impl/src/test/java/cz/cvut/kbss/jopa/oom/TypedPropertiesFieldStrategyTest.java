/**
 * Copyright (C) 2023 Czech Technical University in Prague
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

import cz.cvut.kbss.jopa.environment.OWLClassP;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.environment.utils.TestEnvironmentUtils;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.PropertiesSpecification;
import cz.cvut.kbss.ontodriver.model.*;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.net.URI;
import java.util.*;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class TypedPropertiesFieldStrategyTest {

    private static final URI PK = Generators.createIndividualIdentifier();

    @Mock
    private EntityMappingHelper mapperMock;

    private PropertiesFieldStrategy<OWLClassP> strategy;
    private AxiomValueGatherer gatherer;

    private OWLClassP entity;

    @BeforeEach
    public void setUp() throws Exception {
        MockitoAnnotations.openMocks(this);
        MetamodelMocks mocks = new MetamodelMocks();

        this.entity = new OWLClassP();
        entity.setUri(PK);
        final Descriptor descriptor = new EntityDescriptor();

        this.strategy = new PropertiesFieldStrategy<>(mocks.forOwlClassP().entityType(),
                mocks.forOwlClassP().properties(), descriptor, mapperMock);
        this.gatherer = new AxiomValueGatherer(NamedResource.create(PK), null);
        gatherer.addValue(Assertion.createClassAssertion(false), new Value<>(PK), null);
    }

    @Test
    public void extractsValuesForPersist() throws Exception {
        final int propCount = 5;
        entity.setProperties(Generators.generateTypedProperties(propCount, propCount));
        when(mapperMock.getOriginalInstance(entity)).thenReturn(null);

        strategy.buildAxiomValuesFromInstance(entity, gatherer);

        final Map<Assertion, Set<Value<?>>> res = OOMTestUtils.getPropertiesToAdd(gatherer);
        assertTrue(TestEnvironmentUtils.assertionsCorrespondToProperties(entity.getProperties(), res));
        assertNull(OOMTestUtils.getPropertiesToRemove(gatherer));
    }

    private OWLClassP createOriginal() {
        final OWLClassP original = new OWLClassP();
        original.setUri(PK);
        if (entity.getProperties() != null) {
            final Map<URI, Set<Object>> copy = new HashMap<>();
            for (Map.Entry<URI, Set<Object>> e : entity.getProperties().entrySet()) {
                copy.put(e.getKey(), new HashSet<>(e.getValue()));
            }
            original.setProperties(copy);
        }
        return original;
    }

    @Test
    public void removesSeveralWholeProperties() throws Exception {
        entity.setProperties(Generators.generateTypedProperties(5, 5));
        when(mapperMock.getOriginalInstance(entity)).thenReturn(createOriginal());
        // Remove one property with values
        final URI property = entity.getProperties().keySet().iterator().next();
        final Set<Object> values = entity.getProperties().get(property);
        entity.getProperties().remove(property);

        strategy.buildAxiomValuesFromInstance(entity, gatherer);
        final Map<Assertion, Set<Value<?>>> result = OOMTestUtils.getPropertiesToRemove(gatherer);
        assertTrue(TestEnvironmentUtils
                .assertionsCorrespondToProperties(Collections.singletonMap(property, values), result));
        assertNull(OOMTestUtils.getPropertiesToAdd(gatherer));
    }

    @Test
    public void removesValuesOfSomeProperties() throws Exception {
        entity.setProperties(Generators.generateTypedProperties(5, 5));
        when(mapperMock.getOriginalInstance(entity)).thenReturn(createOriginal());
        final Map<URI, Set<Object>> toRemove = new HashMap<>();
        int propIndex = 0;
        for (Map.Entry<URI, Set<Object>> e : entity.getProperties().entrySet()) {
            if (propIndex++ % 2 != 0) {
                toRemove.put(e.getKey(), new HashSet<>());
                int valueIndex = 0;
                final Iterator<Object> it = e.getValue().iterator();
                while (it.hasNext()) {
                    final Object val = it.next();
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
        entity.setProperties(Generators.generateTypedProperties(3, 3));
        when(mapperMock.getOriginalInstance(entity)).thenReturn(createOriginal());
        final Map<URI, Set<Object>> added = new HashMap<>();
        added.put(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#added"),
                new HashSet<>(Arrays.asList(true, "two", 3)));
        entity.getProperties().putAll(added);

        strategy.buildAxiomValuesFromInstance(entity, gatherer);

        final Map<Assertion, Set<Value<?>>> result = OOMTestUtils.getPropertiesToAdd(gatherer);
        assertTrue(TestEnvironmentUtils.assertionsCorrespondToProperties(added, result));
        assertNull(OOMTestUtils.getPropertiesToRemove(gatherer));
    }

    @Test
    public void addsValuesForExistingProperty() throws Exception {
        entity.setProperties(Generators.generateTypedProperties(3, 3));
        when(mapperMock.getOriginalInstance(entity)).thenReturn(createOriginal());
        final Map<URI, Set<Object>> added = new HashMap<>();
        final URI property = entity.getProperties().keySet().iterator().next();
        final Set<Object> newValues = new HashSet<>(Arrays.asList(false, 7, new Date()));
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
        entity.setProperties(Generators.generateTypedProperties(3, 3));
        when(mapperMock.getOriginalInstance(entity)).thenReturn(createOriginal());
        final Map<URI, Set<Object>> removed = prepareForRemove();
        final Map<URI, Set<Object>> added = prepareForAdd();

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
        entity.setProperties(Generators.generateTypedProperties(3, 3));
        strategy.buildAxiomValuesFromInstance(entity, gatherer);
        final Map<Assertion, Set<Value<?>>> resultAdded = OOMTestUtils.getPropertiesToAdd(gatherer);
        assertTrue(TestEnvironmentUtils.assertionsCorrespondToProperties(entity.getProperties(), resultAdded));
    }

    private Map<URI, Set<Object>> prepareForAdd() {
        final Map<URI, Set<Object>> added = new HashMap<>();
        final URI property = entity.getProperties().keySet().iterator().next();
        final Set<Object> newValues = new HashSet<>(
                Arrays.asList(URI.create("http://krizik.felk.cvut.cz/ontologies/test#A"),
                        URI.create("http://krizik.felk.cvut.cz/ontologies/test#B")));
        added.put(property, newValues);
        entity.getProperties().get(property).addAll(newValues);
        final Map<URI, Set<Object>> newProperty = Collections
                .singletonMap(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#newProperty"),
                        new HashSet<>(Arrays.asList("blablaOne", 2, URI.create("http://blabla.org"))));
        added.putAll(newProperty);
        entity.getProperties().putAll(newProperty);
        return added;
    }

    private Map<URI, Set<Object>> prepareForRemove() {
        final Map<URI, Set<Object>> removed = new HashMap<>();
        URI propertyToUpdate = null;
        URI propertyToRemove = null;
        for (URI uri : entity.getProperties().keySet()) {
            propertyToUpdate = propertyToRemove;
            propertyToRemove = uri;
        }
        removed.put(propertyToRemove, entity.getProperties().get(propertyToRemove));
        entity.getProperties().remove(propertyToRemove);
        final Set<Object> removedValues = new HashSet<>();
        final Iterator<Object> valueIt = entity.getProperties().get(propertyToUpdate).iterator();
        int ind = 0;
        while (valueIt.hasNext()) {
            Object val = valueIt.next();
            if (ind++ % 2 != 0) {
                valueIt.remove();
                removedValues.add(val);
            }
        }
        removed.put(propertyToUpdate, removedValues);
        return removed;
    }

    @Test
    public void buildsInstanceFieldFromTypedAxiomValues() {
        final Map<URI, Set<Object>> properties = Generators.generateTypedProperties();
        final Collection<Axiom<?>> axioms = createAxiomsForProperties(properties);
        axioms.forEach(ax -> strategy.addValueFromAxiom(ax));
        strategy.buildInstanceFieldValue(entity);
        assertEquals(properties, entity.getProperties());
    }

    private Collection<Axiom<?>> createAxiomsForProperties(Map<URI, Set<Object>> data) {
        final NamedResource subject = NamedResource.create(PK);
        final Collection<Axiom<?>> axioms = new ArrayList<>();
        for (Map.Entry<URI, Set<Object>> e : data.entrySet()) {
            axioms.addAll(e.getValue().stream()
                    .map(val -> new AxiomImpl<>(subject, Assertion.createPropertyAssertion(e.getKey(), false),
                            new Value<>(val))).collect(Collectors.toList()));
        }
        return axioms;
    }

    @Test
    public void buildInstanceFieldFromEmptyAxiomsLeavesItNull() {
        strategy.buildInstanceFieldValue(entity);
        assertNull(entity.getProperties());
    }

    @SuppressWarnings("unchecked")
    @Test
    public void buildInstanceFieldFromAxiomsWithIncompatibleValueTypeThrowsIllegalArgumentException() throws Exception {
        final PropertiesSpecification<OWLClassP, ?, URI, Integer> spec = mock(PropertiesSpecification.class);
        when(spec.getPropertyIdentifierType()).thenReturn(URI.class);
        when(spec.getPropertyValueType()).thenReturn(Integer.class);
        final Axiom<?> axiom = new AxiomImpl<>(NamedResource.create(PK),
                Assertion.createPropertyAssertion(URI.create("http://krizik.felk.cvut.cz/test"), false),
                new Value<>(new Date()));
        MetamodelMocks mocks = new MetamodelMocks();
        final PropertiesFieldStrategy<OWLClassP> s = new PropertiesFieldStrategy<>(mocks.forOwlClassP().entityType(),
                spec, new EntityDescriptor(), mapperMock);
        assertThrows(IllegalArgumentException.class, () -> s.addValueFromAxiom(axiom));
    }
}

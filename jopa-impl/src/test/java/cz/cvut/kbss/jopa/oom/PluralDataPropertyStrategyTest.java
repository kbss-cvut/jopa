/**
 * Copyright (C) 2016 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License along with this program. If not, see
 * <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.environment.OWLClassM;
import cz.cvut.kbss.jopa.environment.Vocabulary;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.model.JOPAPersistenceProperties;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.utils.Configuration;
import cz.cvut.kbss.ontodriver.descriptor.AxiomValueDescriptor;
import cz.cvut.kbss.ontodriver.model.*;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.net.URI;
import java.util.*;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.when;

class PluralDataPropertyStrategyTest {

    private static final URI PK = Generators.createIndividualIdentifier();
    private static final NamedResource INDIVIDUAL = NamedResource.create(PK);


    @Mock
    private EntityMappingHelper mapperMock;

    private AxiomValueGatherer gatherer;

    private MetamodelMocks mocks;
    private Descriptor descriptor = new EntityDescriptor();

    @BeforeEach
    void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        final Configuration configuration = new Configuration(
                Collections.singletonMap(JOPAPersistenceProperties.LANG, "en"));
        when(mapperMock.getConfiguration()).thenReturn(configuration);

        this.gatherer = new AxiomValueGatherer(INDIVIDUAL, null);
        this.mocks = new MetamodelMocks();
        when(mapperMock.getEntityType(OWLClassM.class)).thenReturn(mocks.forOwlClassM().entityType());
    }

    @Test
    void buildFieldValueCreatesCorrectCollectionTypeForSet() {
        final PluralDataPropertyStrategy<OWLClassM> strategy = createStrategyForM();
        strategy.addValueFromAxiom(createMSetAxiom());
        final OWLClassM m = new OWLClassM();
        strategy.buildInstanceFieldValue(m);
        assertNotNull(m.getIntegerSet());
    }

    private PluralDataPropertyStrategy<OWLClassM> createStrategyForM() {
        return new PluralDataPropertyStrategy<>(mocks.forOwlClassM().entityType(),
                mocks.forOwlClassM().integerSetAttribute(), descriptor, mapperMock);
    }

    private Axiom<Integer> createMSetAxiom() {
        return new AxiomImpl<>(INDIVIDUAL,
                assertionForMIntegerSet(),
                new Value<>(Generators.randomInt()));
    }

    private Assertion assertionForMIntegerSet() {
        return Assertion.createDataPropertyAssertion(URI.create(Vocabulary.p_m_IntegerSet), false);
    }

    @Test
    void addValueFromAxiomAddsAllValuesToTheCollection() {
        final PluralDataPropertyStrategy<OWLClassM> strategy = createStrategyForM();
        final Set<Integer> values = addValuesFromAxioms(strategy);

        final OWLClassM m = new OWLClassM();
        strategy.buildInstanceFieldValue(m);
        assertEquals(values, m.getIntegerSet());
    }

    private Set<Integer> addValuesFromAxioms(PluralDataPropertyStrategy<OWLClassM> strategy) {
        final Set<Integer> values = new HashSet<>();
        for (int i = 0; i < Generators.randomPositiveInt(10); i++) {
            final Axiom<Integer> axiom = createMSetAxiom();
            values.add(axiom.getValue().getValue());
            strategy.addValueFromAxiom(axiom);
        }
        return values;
    }

    @Test
    void addValueFromAxiomSkipsValuesWithInvalidRange() {
        final PluralDataPropertyStrategy<OWLClassM> strategy = createStrategyForM();
        final Set<Integer> values = addValuesFromAxioms(strategy);
        final Assertion a = assertionForMIntegerSet();
        strategy.addValueFromAxiom(new AxiomImpl<>(INDIVIDUAL, a, new Value<>("Test")));
        strategy.addValueFromAxiom(
                new AxiomImpl<>(INDIVIDUAL, a, new Value<>(Generators.createIndividualIdentifier())));
        strategy.addValueFromAxiom(new AxiomImpl<>(INDIVIDUAL, a, new Value<>(new Date())));

        final OWLClassM m = new OWLClassM();
        strategy.buildInstanceFieldValue(m);
        assertEquals(values, m.getIntegerSet());
    }

    @Test
    void buildFieldValueDoesNothingWhenNoValuesWereAdded() {
        final PluralDataPropertyStrategy<OWLClassM> strategy = createStrategyForM();
        final OWLClassM m = new OWLClassM();
        strategy.buildInstanceFieldValue(m);
        assertNull(m.getIntegerSet());
    }

    @Test
    void buildAxiomValuesFromInstanceAddsAllValuesIntoAxiomValueGatherer() throws Exception {
        final PluralDataPropertyStrategy<OWLClassM> strategy = createStrategyForM();
        final OWLClassM m = new OWLClassM();
        m.initializeTestValues(false);
        strategy.buildAxiomValuesFromInstance(m, gatherer);
        final AxiomValueDescriptor valueDescriptor = OOMTestUtils.getAxiomValueDescriptor(gatherer);
        assertEquals(1, valueDescriptor.getAssertions().size());
        assertTrue(valueDescriptor.getAssertions().contains(assertionForMIntegerSet()));
        final List<Value<?>> values = valueDescriptor.getAssertionValues(assertionForMIntegerSet());
        assertEquals(m.getIntegerSet().size(), values.size());
        values.forEach(v -> assertTrue(m.getIntegerSet().contains((Integer) v.getValue())));
    }

    @Test
    void buildAxiomValuesFromInstancesAddsNullValueWhenAttributeValueIsNull() throws Exception {
        final PluralDataPropertyStrategy<OWLClassM> strategy = createStrategyForM();
        final OWLClassM m = new OWLClassM();

        strategy.buildAxiomValuesFromInstance(m, gatherer);

        final AxiomValueDescriptor valueDescriptor = OOMTestUtils.getAxiomValueDescriptor(gatherer);
        assertEquals(1, valueDescriptor.getAssertions().size());
        assertTrue(valueDescriptor.getAssertions().contains(assertionForMIntegerSet()));
        final List<Value<?>> values = valueDescriptor.getAssertionValues(assertionForMIntegerSet());
        assertEquals(1, values.size());
        assertTrue(values.contains(Value.nullValue()));
    }

    @Test
    void buildAxiomValuesFromInstanceAddsNullValueWhenAttributeValueIsEmptyCollection() throws Exception {
        final PluralDataPropertyStrategy<OWLClassM> strategy = createStrategyForM();
        final OWLClassM m = new OWLClassM();
        m.setIntegerSet(Collections.emptySet());

        strategy.buildAxiomValuesFromInstance(m, gatherer);

        final AxiomValueDescriptor valueDescriptor = OOMTestUtils.getAxiomValueDescriptor(gatherer);
        assertEquals(1, valueDescriptor.getAssertions().size());
        assertTrue(valueDescriptor.getAssertions().contains(assertionForMIntegerSet()));
        final List<Value<?>> values = valueDescriptor.getAssertionValues(assertionForMIntegerSet());
        assertEquals(1, values.size());
        assertTrue(values.contains(Value.nullValue()));
    }

    @Test
    void buildAxiomsSetsLanguageTagAccordingToDescriptorLanguage() throws Exception {
        descriptor.setLanguage("en");
        buildAxiomsAndVerifyLanguageTag();
    }

    private void buildAxiomsAndVerifyLanguageTag() throws Exception {
        // Yes, the plural attribute contains integers, but it is not important on this level
        final PluralDataPropertyStrategy<OWLClassM> strategy = createStrategyForM();
        final OWLClassM m = new OWLClassM();
        m.setIntegerSet(Collections.singleton(117));

        final AxiomValueGatherer builder = new AxiomValueGatherer(NamedResource.create(PK), null);
        strategy.buildAxiomValuesFromInstance(m, builder);
        final AxiomValueDescriptor valueDescriptor = OOMTestUtils.getAxiomValueDescriptor(builder);
        assertEquals(1, valueDescriptor.getAssertions().size());
        final Assertion assertion = valueDescriptor.getAssertions().iterator().next();
        assertTrue(assertion.hasLanguage());
        assertEquals("en", assertion.getLanguage());
    }

    @Test
    void buildAxiomsSetsLanguageTagAccordingToPUConfigurationWhenItIsNotSpecifiedInDescriptor()
            throws Exception {
        buildAxiomsAndVerifyLanguageTag();
    }

    @Test
    void addValueFromAxiomUsesConverterToTransformValueToCorrectType() {
        final PluralDataPropertyStrategy<OWLClassM> strategy = createStrategyForM();
        final Axiom<Short> axiom = new AxiomImpl<>(INDIVIDUAL, assertionForMIntegerSet(), new Value<>((short) 117));
        strategy.addValueFromAxiom(axiom);
        final OWLClassM m = new OWLClassM();
        strategy.buildInstanceFieldValue(m);
        assertEquals(Collections.singleton(117), m.getIntegerSet());
    }
}

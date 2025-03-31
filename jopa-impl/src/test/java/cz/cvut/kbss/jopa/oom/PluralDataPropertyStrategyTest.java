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
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.AxiomImpl;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.net.URI;
import java.util.Collections;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasItem;
import static org.hamcrest.Matchers.not;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class PluralDataPropertyStrategyTest {

    private static final String LANG = "en";
    private static final URI PK = Generators.createIndividualIdentifier();
    private static final NamedResource INDIVIDUAL = NamedResource.create(PK);

    @Mock
    private EntityMappingHelper mapperMock;

    private AxiomValueGatherer gatherer;

    private MetamodelMocks mocks;
    private final Descriptor descriptor = new EntityDescriptor();

    @BeforeEach
    void setUp() throws Exception {
        final Configuration configuration = new Configuration(
                Collections.singletonMap(JOPAPersistenceProperties.LANG, LANG));
        when(mapperMock.getConfiguration()).thenReturn(configuration);

        this.gatherer = new AxiomValueGatherer(INDIVIDUAL, null);
        this.mocks = new MetamodelMocks();
        when(mapperMock.getEntityType(OWLClassM.class)).thenReturn(mocks.forOwlClassM().entityType());
    }

    @Test
    void buildFieldValueCreatesCorrectCollectionTypeForSet() {
        final PluralDataPropertyStrategy<OWLClassM> strategy = createStrategyForM();
        strategy.addAxiomValue(createMSetAxiom());
        final OWLClassM m = new OWLClassM();
        strategy.buildInstanceFieldValue(m);
        assertNotNull(m.getIntegerSet());
    }

    private PluralDataPropertyStrategy<OWLClassM> createStrategyForM() {
        return new PluralDataPropertyStrategy<>(mocks.forOwlClassM().entityType(),
                                                mocks.forOwlClassM().integerSetAttribute(), descriptor, mapperMock);
    }

    private Axiom<Integer> createMSetAxiom() {
        return new AxiomImpl<>(INDIVIDUAL, assertionForMIntegerSet(), new Value<>(Generators.randomInt()));
    }

    private Assertion assertionForMIntegerSet() {
        return Assertion.createDataPropertyAssertion(URI.create(Vocabulary.p_m_IntegerSet), LANG, false);
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
            strategy.addAxiomValue(axiom);
        }
        return values;
    }

    @Test
    void addValueFromAxiomSkipsValuesWithInvalidRange() {
        final PluralDataPropertyStrategy<OWLClassM> strategy = createStrategyForM();
        final Set<Integer> values = addValuesFromAxioms(strategy);
        final Assertion a = assertionForMIntegerSet();
        strategy.addAxiomValue(new AxiomImpl<>(INDIVIDUAL, a, new Value<>("Test")));
        strategy.addAxiomValue(
                new AxiomImpl<>(INDIVIDUAL, a, new Value<>(Generators.createIndividualIdentifier())));
        strategy.addAxiomValue(new AxiomImpl<>(INDIVIDUAL, a, new Value<>(new Date())));

        final OWLClassM m = new OWLClassM();
        strategy.buildInstanceFieldValue(m);
        assertEquals(values, m.getIntegerSet());
    }

    @Test
    void buildFieldValueSetsEmptyCollectionWhenNoValuesWereAdded() {
        final PluralDataPropertyStrategy<OWLClassM> strategy = createStrategyForM();
        final OWLClassM m = new OWLClassM();
        strategy.buildInstanceFieldValue(m);
        assertNotNull(m.getIntegerSet());
        assertTrue(m.getIntegerSet().isEmpty());
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
        descriptor.setLanguage("cs");
        buildAxiomsAndVerifyLanguageTag("cs");
    }

    private void buildAxiomsAndVerifyLanguageTag(String expectedLang) throws Exception {
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
        assertEquals(expectedLang, assertion.getLanguage());
    }

    @Test
    void buildAxiomsSetsLanguageTagAccordingToPUConfigurationWhenItIsNotSpecifiedInDescriptor()
            throws Exception {
        buildAxiomsAndVerifyLanguageTag(LANG);
    }

    @Test
    void addValueFromAxiomUsesConverterToTransformValueToCorrectType() {
        final PluralDataPropertyStrategy<OWLClassM> strategy = createStrategyForM();
        final Axiom<Short> axiom = new AxiomImpl<>(INDIVIDUAL, assertionForMIntegerSet(), new Value<>((short) 117));
        strategy.addAxiomValue(axiom);
        final OWLClassM m = new OWLClassM();
        strategy.buildInstanceFieldValue(m);
        assertEquals(Collections.singleton(117), m.getIntegerSet());
    }

    @Test
    void buildAxiomsFromInstanceReturnsAxiomsCorrespondingToAttributeValue() {
        final PluralDataPropertyStrategy<OWLClassM> sut = createStrategyForM();
        final OWLClassM m = new OWLClassM();
        m.initializeTestValues(true);
        final Set<Axiom<?>> result = sut.buildAxiomsFromInstance(m);
        assertEquals(m.getIntegerSet().size(), result.size());
        final Assertion assertion = assertionForMIntegerSet();
        m.getIntegerSet().forEach(i -> assertThat(result, hasItem(
                new AxiomImpl<>(NamedResource.create(m.getKey()), assertion, new Value<>(i))
        )));
    }

    @Test
    void buildAxiomsFromInstanceReturnsAxiomEmptySetWhenCollectionIsEmpty() {
        final PluralDataPropertyStrategy<OWLClassM> sut = createStrategyForM();
        final OWLClassM m = new OWLClassM();
        m.initializeTestValues(true);
        m.setIntegerSet(Collections.emptySet());
        final Set<Axiom<?>> result = sut.buildAxiomsFromInstance(m);
        assertNotNull(result);
        assertTrue(result.isEmpty());
    }

    @Test
    void buildAxiomValuesFromInstanceFiltersOutInferredValues() throws Exception {
        when(mocks.forOwlClassM().integerSetAttribute().isInferred()).thenReturn(true);
        final PluralDataPropertyStrategy<OWLClassM> sut = createStrategyForM();
        final OWLClassM m = new OWLClassM();
        m.initializeTestValues(true);
        final Set<Integer> inferred = m.getIntegerSet().stream().filter(i -> Generators.randomBoolean()).collect(
                Collectors.toSet());
        doAnswer(args -> inferred.contains((Integer) args.getArgument(0, Axiom.class).getValue().getValue())).when(
                mapperMock).isInferred(any(Axiom.class), any());

        sut.buildAxiomValuesFromInstance(m, gatherer);
        final AxiomValueDescriptor axiomDescriptor = OOMTestUtils.getAxiomValueDescriptor(gatherer);
        final List<Value<?>> values = axiomDescriptor.getAssertionValues(
                Assertion.createDataPropertyAssertion(URI.create(Vocabulary.p_m_IntegerSet), LANG, true));
        if (inferred.size() < m.getIntegerSet().size()) {
            assertFalse(values.isEmpty());
        }
        inferred.forEach(i -> assertThat(values, not(hasItem(new Value<>(i)))));
    }
}

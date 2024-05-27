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
package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassM;
import cz.cvut.kbss.jopa.environment.OWLClassT;
import cz.cvut.kbss.jopa.environment.Vocabulary;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.AxiomValueDescriptor;
import cz.cvut.kbss.ontodriver.model.*;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.net.URI;
import java.time.OffsetDateTime;
import java.time.ZoneOffset;
import java.util.Collections;
import java.util.Set;

import static org.hamcrest.CoreMatchers.hasItem;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.*;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class SingularDataPropertyStrategyTest {

    private static final URI PK = Generators.createIndividualIdentifier();

    @Mock
    private EntityMappingHelper mapperMock;

    private MetamodelMocks mocks;

    private final Descriptor descriptor = new EntityDescriptor();

    @BeforeEach
    void setUp() throws Exception {
        this.mocks = new MetamodelMocks();
    }

    @Test
    void buildAxiomsSetsLanguageTagAccordingToDescriptorLanguage() throws Exception {
        descriptor.setLanguage("en");
        buildAxiomsAndVerifyLanguageTag();
    }

    private void buildAxiomsAndVerifyLanguageTag() throws Exception {
        final SingularDataPropertyStrategy<OWLClassA> strategy = new SingularDataPropertyStrategy<>(
                mocks.forOwlClassA().entityType(), mocks.forOwlClassA().stringAttribute(), descriptor, mapperMock);
        final OWLClassA a = new OWLClassA();
        a.setUri(PK);
        a.setStringAttribute("english");

        final AxiomValueGatherer builder = new AxiomValueGatherer(NamedResource.create(PK), null);
        strategy.buildAxiomValuesFromInstance(a, builder);
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
    void buildInstanceFieldValueTransformsOffsetDateTimeToLocalDateTime() {
        final SingularDataPropertyStrategy<OWLClassT> strategy = new SingularDataPropertyStrategy<>(
                mocks.forOwlClassT().entityType(), mocks.forOwlClassT().tLocalDateTimeAtt(), descriptor, mapperMock);
        final OWLClassT t = new OWLClassT();
        t.setUri(PK);

        final OffsetDateTime value = OffsetDateTime.now();
        final Axiom<OffsetDateTime> axiom = new AxiomImpl<>(NamedResource.create(PK), strategy.createAssertion(),
                new Value<>(value));
        strategy.addAxiomValue(axiom);
        strategy.buildInstanceFieldValue(t);
        assertNotNull(t.getLocalDateTime());
        assertEquals(value.toLocalDateTime(), t.getLocalDateTime());
    }

    @Test
    void buildInstanceFieldConvertsRepositoryValueToEnum() {
        final SingularDataPropertyStrategy<OWLClassM> sut =
                new SingularDataPropertyStrategy<>(mocks.forOwlClassM().entityType(),
                        mocks.forOwlClassM().enumAttribute(), descriptor, mapperMock);
        final OWLClassM m = new OWLClassM();
        m.setKey(PK.toString());

        final Axiom<String> axiom = new AxiomImpl<>(NamedResource.create(PK), sut.createAssertion(),
                new Value<>(OWLClassM.Severity.MEDIUM.toString()));
        sut.addAxiomValue(axiom);
        sut.buildInstanceFieldValue(m);
        assertEquals(OWLClassM.Severity.MEDIUM, m.getEnumAttribute());
    }

    @Test
    void buildInstanceFieldTransformsValueToLexicalForm() {
        final SingularDataPropertyStrategy<OWLClassM> sut = new SingularDataPropertyStrategy<>(
                mocks.forOwlClassM().entityType(),
                mocks.forOwlClassM().lexicalFormAttribute(), descriptor, mapperMock);
        final OWLClassM m = new OWLClassM();
        m.setKey(PK.toString());

        final Integer value = 117;
        final Axiom<Integer> axiom = new AxiomImpl<>(NamedResource.create(PK), sut.createAssertion(),
                new Value<>(value));
        sut.addAxiomValue(axiom);
        sut.buildInstanceFieldValue(m);
        assertEquals(value.toString(), m.getLexicalForm());
    }

    @Test
    void buildAxiomValuesFromInstanceHandlesNullEnumAttributeValue() throws Exception {
        final SingularDataPropertyStrategy<OWLClassM> sut =
                new SingularDataPropertyStrategy<>(mocks.forOwlClassM().entityType(),
                        mocks.forOwlClassM().enumAttribute(), descriptor, mapperMock);
        final OWLClassM m = new OWLClassM();
        m.setKey(PK.toString());
        final AxiomValueGatherer builder = new AxiomValueGatherer(NamedResource.create(PK), null);

        sut.buildAxiomValuesFromInstance(m, builder);
        final AxiomValueDescriptor valueDescriptor = OOMTestUtils.getAxiomValueDescriptor(builder);
        final Assertion a = Assertion.createDataPropertyAssertion(URI.create(Vocabulary.p_m_enumAttribute), Generators.LANG, false);
        assertThat(valueDescriptor.getAssertions(), hasItem(a));
        assertEquals(Collections.singletonList(Value.nullValue()), valueDescriptor.getAssertionValues(a));
    }

    @Test
    void buildInstanceFieldTransformsUsingCustomConverter() {
        final SingularDataPropertyStrategy<OWLClassM> sut = new SingularDataPropertyStrategy<>(
                mocks.forOwlClassM().entityType(),
                mocks.forOwlClassM().withConverterAttribute(), descriptor, mapperMock);
        final OWLClassM m = new OWLClassM();
        m.setKey(PK.toString());

        final String value = "-01:00";
        final Axiom<String> axiom = new AxiomImpl<>(NamedResource.create(PK), sut.createAssertion(),
                                                     new Value<>(value));
        sut.addAxiomValue(axiom);
        sut.buildInstanceFieldValue(m);
        assertEquals(ZoneOffset.of(value), m.getWithConverter());
    }

    @Test
    void buildAxiomsFromInstanceReturnsAxiomsCorrespondingToAttributeValue() {
        final SingularDataPropertyStrategy<OWLClassA> strategy = new SingularDataPropertyStrategy<>(
                mocks.forOwlClassA().entityType(), mocks.forOwlClassA().stringAttribute(), descriptor, mapperMock);
        final OWLClassA a = Generators.generateOwlClassAInstance();

        final Set<Axiom<?>> result = strategy.buildAxiomsFromInstance(a);
        assertEquals(1, result.size());
        final Axiom<?> ax = result.iterator().next();
        assertEquals(NamedResource.create(a.getUri()), ax.getSubject());
        assertEquals(mocks.forOwlClassA().stringAttribute().getIRI().toURI(), ax.getAssertion().getIdentifier());
        assertEquals(a.getStringAttribute(), ax.getValue().getValue());
    }

    @Test
    void buildAxiomsFromInstanceReturnsEmptyCollectionWhenAttributeValueIsNull() {
        final SingularDataPropertyStrategy<OWLClassA> strategy = new SingularDataPropertyStrategy<>(
                mocks.forOwlClassA().entityType(), mocks.forOwlClassA().stringAttribute(), descriptor, mapperMock);
        final OWLClassA a = Generators.generateOwlClassAInstance();
        a.setStringAttribute(null);

        final Set<Axiom<?>> result = strategy.buildAxiomsFromInstance(a);
        assertNotNull(result);
        assertTrue(result.isEmpty());
    }
}

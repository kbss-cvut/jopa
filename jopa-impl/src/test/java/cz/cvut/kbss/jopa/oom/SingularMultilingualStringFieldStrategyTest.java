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

import cz.cvut.kbss.jopa.environment.OWLClassU;
import cz.cvut.kbss.jopa.environment.Vocabulary;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.exceptions.CardinalityConstraintViolatedException;
import cz.cvut.kbss.jopa.model.MultilingualString;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.AxiomValueDescriptor;
import cz.cvut.kbss.ontodriver.model.*;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.net.URI;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static cz.cvut.kbss.jopa.environment.utils.Generators.LANG;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasItem;
import static org.junit.jupiter.api.Assertions.*;

class SingularMultilingualStringFieldStrategyTest {

    private static final URI ID = Generators.createIndividualIdentifier();
    private static final NamedResource INDIVIDUAL = NamedResource.create(ID);

    @Mock
    private EntityMappingHelper mapperMock;

    private MetamodelMocks mocks;

    private final Descriptor descriptor = new EntityDescriptor();

    @BeforeEach
    void setUp() throws Exception {
        MockitoAnnotations.openMocks(this);
        this.mocks = new MetamodelMocks();
    }

    @Test
    void addValueFromAxiomCreatesMultilingualStringInstanceForLangStringValue() {
        final SingularMultilingualStringFieldStrategy<OWLClassU> sut = createStrategy();
        final String value = "test";
        final Axiom<LangString> axiom = new AxiomImpl<>(INDIVIDUAL, Assertion.createDataPropertyAssertion(URI.create(
                Vocabulary.P_U_SINGULAR_MULTILINGUAL_ATTRIBUTE), false),
                                                        new Value<>(new LangString(value, LANG)));

        sut.addAxiomValue(axiom);
        final OWLClassU instance = new OWLClassU();
        sut.buildInstanceFieldValue(instance);
        assertNotNull(instance.getSingularStringAtt());
        assertTrue(instance.getSingularStringAtt().contains(LANG));
        assertEquals(value, instance.getSingularStringAtt().get(LANG));
    }

    private SingularMultilingualStringFieldStrategy<OWLClassU> createStrategy() {
        return new SingularMultilingualStringFieldStrategy<>(mocks.forOwlClassU().entityType(),
                                                             mocks.forOwlClassU().uSingularStringAtt(), descriptor,
                                                             mapperMock);
    }

    @Test
    void addValueFromAxiomSupportsAddingMultipleValuesWithDifferentLanguageTags() {
        final SingularMultilingualStringFieldStrategy<OWLClassU> sut = createStrategy();
        final Assertion assertion = Assertion
                .createDataPropertyAssertion(URI.create(Vocabulary.P_U_SINGULAR_MULTILINGUAL_ATTRIBUTE), false);
        final String valueEn = "car";
        final Axiom<LangString> axiomEn = new AxiomImpl<>(INDIVIDUAL, assertion,
                                                          new Value<>(new LangString(valueEn, LANG)));
        final String valueCs = "automobil";
        final Axiom<LangString> axiomCs = new AxiomImpl<>(INDIVIDUAL, assertion,
                                                          new Value<>(new LangString(valueCs, "cs")));

        sut.addAxiomValue(axiomEn);
        sut.addAxiomValue(axiomCs);
        final OWLClassU instance = new OWLClassU();
        sut.buildInstanceFieldValue(instance);
        assertNotNull(instance.getSingularStringAtt());
        assertTrue(instance.getSingularStringAtt().contains(LANG));
        assertEquals(valueEn, instance.getSingularStringAtt().get(LANG));
        assertTrue(instance.getSingularStringAtt().contains("cs"));
        assertEquals(valueCs, instance.getSingularStringAtt().get("cs"));
    }

    @Test
    void addValueFromAxiomSupportsAddingStringValueWithoutLanguageTag() {
        final SingularMultilingualStringFieldStrategy<OWLClassU> sut = createStrategy();
        final String value = "test";
        final Axiom<String> axiom = new AxiomImpl<>(INDIVIDUAL, Assertion.createDataPropertyAssertion(URI.create(
                Vocabulary.P_U_SINGULAR_MULTILINGUAL_ATTRIBUTE), false),
                                                    new Value<>(value));

        sut.addAxiomValue(axiom);
        final OWLClassU instance = new OWLClassU();
        sut.buildInstanceFieldValue(instance);
        assertNotNull(instance.getSingularStringAtt());
        assertTrue(instance.getSingularStringAtt().contains(null));
        assertEquals(value, instance.getSingularStringAtt().get());
    }

    @Test
    void addValueFromAxiomThrowsCardinalityConstraintViolatedExceptionWhenMultipleValuesOfSameLanguageAreAdded() {
        final SingularMultilingualStringFieldStrategy<OWLClassU> sut = createStrategy();
        final String value = "test";
        final Axiom<LangString> axiomOne = new AxiomImpl<>(INDIVIDUAL, Assertion.createDataPropertyAssertion(URI.create(
                Vocabulary.P_U_SINGULAR_MULTILINGUAL_ATTRIBUTE), false),
                                                           new Value<>(new LangString(value, LANG)));
        final Axiom<LangString> axiomTwo = new AxiomImpl<>(INDIVIDUAL, Assertion.createDataPropertyAssertion(URI.create(
                Vocabulary.P_U_SINGULAR_MULTILINGUAL_ATTRIBUTE), false),
                                                           new Value<>(new LangString("test two", LANG)));

        sut.addAxiomValue(axiomOne);
        assertThrows(CardinalityConstraintViolatedException.class, () -> sut.addAxiomValue(axiomTwo));
    }

    @Test
    void addValueFromAxiomSupportsAddingLanguageTaggedAndLanguageLessValues() {
        final SingularMultilingualStringFieldStrategy<OWLClassU> sut = createStrategy();
        final Assertion assertion = Assertion
                .createDataPropertyAssertion(URI.create(Vocabulary.P_U_SINGULAR_MULTILINGUAL_ATTRIBUTE), false);
        final String valueEn = "car";
        final Axiom<LangString> axiomEn = new AxiomImpl<>(INDIVIDUAL, assertion,
                                                          new Value<>(new LangString(valueEn, LANG)));
        final String value = "auto";
        final Axiom<LangString> axiomCs = new AxiomImpl<>(INDIVIDUAL, assertion,
                                                          new Value<>(new LangString(value)));

        sut.addAxiomValue(axiomEn);
        sut.addAxiomValue(axiomCs);
        final OWLClassU instance = new OWLClassU();
        sut.buildInstanceFieldValue(instance);
        assertNotNull(instance.getSingularStringAtt());
        assertTrue(instance.getSingularStringAtt().contains(LANG));
        assertEquals(valueEn, instance.getSingularStringAtt().get(LANG));
        assertTrue(instance.getSingularStringAtt().contains(null));
        assertEquals(value, instance.getSingularStringAtt().get());
    }

    @Test
    void addValueFromAxiomThrowsCardinalityConstraintViolatedExceptionWhenMultipleSimpleLiteralValuesAreAdded() {
        final SingularMultilingualStringFieldStrategy<OWLClassU> sut = createStrategy();
        final Axiom<String> axiomOne = new AxiomImpl<>(INDIVIDUAL, Assertion.createDataPropertyAssertion(URI.create(
                Vocabulary.P_U_SINGULAR_MULTILINGUAL_ATTRIBUTE), false),
                                                       new Value<>("test one"));
        final Axiom<LangString> axiomTwo = new AxiomImpl<>(INDIVIDUAL, Assertion.createDataPropertyAssertion(URI.create(
                Vocabulary.P_U_SINGULAR_MULTILINGUAL_ATTRIBUTE), false),
                                                           new Value<>(new LangString("test two")));

        sut.addAxiomValue(axiomOne);
        assertThrows(CardinalityConstraintViolatedException.class, () -> sut.addAxiomValue(axiomTwo));
    }

    @Test
    void buildAxiomValuesFromInstanceAddsAllTranslationsToValueDescriptor() throws Exception {
        final SingularMultilingualStringFieldStrategy<OWLClassU> sut = createStrategy();
        final OWLClassU instance = new OWLClassU(ID);
        instance.setSingularStringAtt(MultilingualString.create("car", LANG));
        instance.getSingularStringAtt().set("cs", "automobil");

        final AxiomValueGatherer builder = new AxiomValueGatherer(INDIVIDUAL, null);
        sut.buildAxiomValuesFromInstance(instance, builder);
        final AxiomValueDescriptor valueDescriptor = OOMTestUtils.getAxiomValueDescriptor(builder);
        assertEquals(1, valueDescriptor.getAssertions().size());
        final List<Value<?>> values = valueDescriptor.getAssertionValues(Assertion
                                                                                 .createDataPropertyAssertion(
                                                                                         URI.create(
                                                                                                 Vocabulary.P_U_SINGULAR_MULTILINGUAL_ATTRIBUTE),
                                                                                         false));
        assertEquals(instance.getSingularStringAtt().getLanguages().size(), values.size());
        for (Map.Entry<String, String> entry : instance.getSingularStringAtt().getValue().entrySet()) {
            assertTrue(values.contains(new Value<>(new LangString(entry.getValue(), entry.getKey()))));
        }
    }

    @Test
    void buildAxiomValuesFromInstanceAddsNullValueWhenAttributeValueIsNull() throws Exception {
        final SingularMultilingualStringFieldStrategy<OWLClassU> sut = createStrategy();
        final OWLClassU instance = new OWLClassU(ID);

        final AxiomValueGatherer builder = new AxiomValueGatherer(INDIVIDUAL, null);
        sut.buildAxiomValuesFromInstance(instance, builder);
        final AxiomValueDescriptor valueDescriptor = OOMTestUtils.getAxiomValueDescriptor(builder);
        assertEquals(1, valueDescriptor.getAssertions().size());
        final List<Value<?>> values = valueDescriptor.getAssertionValues(Assertion
                                                                                 .createDataPropertyAssertion(
                                                                                         URI.create(
                                                                                                 Vocabulary.P_U_SINGULAR_MULTILINGUAL_ATTRIBUTE),
                                                                                         false));
        assertEquals(1, values.size());
        assertEquals(Value.nullValue(), values.get(0));
    }

    @Test
    void buildAxiomValuesFromInstanceAddsNullValueWhenAttributeValueIsEmpty() throws Exception {
        final SingularMultilingualStringFieldStrategy<OWLClassU> sut = createStrategy();
        final OWLClassU instance = new OWLClassU(ID);
        instance.setSingularStringAtt(new MultilingualString());
        assertTrue(instance.getSingularStringAtt().isEmpty());

        final AxiomValueGatherer builder = new AxiomValueGatherer(INDIVIDUAL, null);
        sut.buildAxiomValuesFromInstance(instance, builder);
        final AxiomValueDescriptor valueDescriptor = OOMTestUtils.getAxiomValueDescriptor(builder);
        assertEquals(1, valueDescriptor.getAssertions().size());
        final List<Value<?>> values = valueDescriptor.getAssertionValues(Assertion
                                                                                 .createDataPropertyAssertion(
                                                                                         URI.create(
                                                                                                 Vocabulary.P_U_SINGULAR_MULTILINGUAL_ATTRIBUTE),
                                                                                         false));
        assertEquals(1, values.size());
        assertEquals(Value.nullValue(), values.get(0));
    }

    @Test
    void buildAxiomsFromInstanceReturnsAxiomsCorrespondingToAttributeValue() {
        final SingularMultilingualStringFieldStrategy<OWLClassU> sut = createStrategy();
        final OWLClassU instance = new OWLClassU(ID);
        instance.setSingularStringAtt(MultilingualString.create("car", LANG));
        instance.getSingularStringAtt().set("cs", "automobil");

        final Set<Axiom<?>> result = sut.buildAxiomsFromInstance(instance);
        assertEquals(instance.getSingularStringAtt().getValue().size(), result.size());
        instance.getSingularStringAtt().getValue()
                .forEach((k, v) -> assertThat(result, hasItem(new AxiomImpl<>(NamedResource.create(ID),
                                                                              Assertion.createDataPropertyAssertion(
                                                                                      mocks.forOwlClassU()
                                                                                           .uSingularStringAtt()
                                                                                           .getIRI()
                                                                                           .toURI(), false),
                                                                              new Value<>(new LangString(v, k))))));
    }

    @Test
    void buildAxiomsFromInstanceReturnsEmptySetWhenValueIsNull() {
        final SingularMultilingualStringFieldStrategy<OWLClassU> sut = createStrategy();
        final OWLClassU instance = new OWLClassU(ID);
        instance.setSingularStringAtt(null);

        final Set<Axiom<?>> result = sut.buildAxiomsFromInstance(instance);
        assertNotNull(result);
        assertTrue(result.isEmpty());
    }
}

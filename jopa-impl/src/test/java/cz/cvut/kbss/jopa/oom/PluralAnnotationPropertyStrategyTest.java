/**
 * Copyright (C) 2019 Czech Technical University in Prague
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

import cz.cvut.kbss.jopa.environment.OWLClassN;
import cz.cvut.kbss.jopa.environment.Vocabulary;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.model.IRI;
import cz.cvut.kbss.jopa.model.JOPAPersistenceProperties;
import cz.cvut.kbss.jopa.model.annotations.OWLAnnotationProperty;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.*;
import cz.cvut.kbss.jopa.oom.converter.ToLexicalFormConverter;
import cz.cvut.kbss.jopa.utils.Configuration;
import cz.cvut.kbss.jopa.vocabulary.DC;
import cz.cvut.kbss.ontodriver.descriptor.AxiomValueDescriptor;
import cz.cvut.kbss.ontodriver.model.*;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.net.URI;
import java.util.Collection;
import java.util.Collections;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class PluralAnnotationPropertyStrategyTest {

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
        when(mapperMock.getEntityType(OWLClassN.class)).thenReturn(mocks.forOwlClassN().entityType());
    }

    @Test
    void createAssertionCreatesAnnotationPropertyAssertion() {
        final PluralAnnotationPropertyStrategy<OWLClassN> sut = strategyForN();
        final Assertion result = sut.createAssertion();
        assertEquals(Assertion.AssertionType.ANNOTATION_PROPERTY, result.getType());
        assertEquals(Vocabulary.DC_SOURCE, result.getIdentifier().toString());
        assertFalse(result.isInferred());
    }

    private PluralAnnotationPropertyStrategy<OWLClassN> strategyForN() {
        return new PluralAnnotationPropertyStrategy<>(mocks.forOwlClassN().entityType(),
                mocks.forOwlClassN().pluralAnnotationAttribute(), descriptor, mapperMock);
    }

    @Test
    void addValueFromAxiomAddsStringValueToValues() {
        final PluralAnnotationPropertyStrategy<OWLClassN> sut = strategyForN();
        final String value = "test";
        final Axiom<String> axiom = new AxiomImpl<>(INDIVIDUAL, createAnnotationAssertionForN(), new Value<>(value));
        sut.addValueFromAxiom(axiom);

        final OWLClassN instance = new OWLClassN();
        sut.buildInstanceFieldValue(instance);
        assertEquals(Collections.singleton(value), instance.getPluralAnnotation());
    }

    private Assertion createAnnotationAssertionForN() {
        return Assertion.createAnnotationPropertyAssertion(URI.create(Vocabulary.DC_SOURCE), false);
    }

    @Test
    void addValueFromAxiomAddsNamedResourceToValues() {
        final PluralAnnotationPropertyStrategy<OWLClassN> sut = strategyForN();
        final NamedResource value = NamedResource.create(Generators.createIndividualIdentifier());
        final Axiom<NamedResource> axiom =
                new AxiomImpl<>(INDIVIDUAL, createAnnotationAssertionForN(), new Value<>(value));
        sut.addValueFromAxiom(axiom);

        final OWLClassN instance = new OWLClassN();
        sut.buildInstanceFieldValue(instance);
        assertEquals(Collections.singleton(value.getIdentifier().toString()), instance.getPluralAnnotation());
    }

    @Test
    void addValueFromAxiomAddsStringToUriValues() throws Exception {
        final PluralAnnotationPropertyStrategy<WithPluralUriAnnotations> sut = createStrategyWithPluralAnnotations(
                WithPluralUriAnnotations.class, URI.class);
        final NamedResource value = NamedResource.create(Generators.createIndividualIdentifier());
        final Axiom<NamedResource> axiom =
                new AxiomImpl<>(INDIVIDUAL, createAnnotationAssertionForN(), new Value<>(value));
        sut.addValueFromAxiom(axiom);

        final WithPluralUriAnnotations instance = new WithPluralUriAnnotations();
        sut.buildInstanceFieldValue(instance);
        assertEquals(Collections.singleton(value.getIdentifier()), instance.sources);
    }

    private <T, X> PluralAnnotationPropertyStrategy<T> createStrategyWithPluralAnnotations(Class<T> entity,
                                                                                           Class<X> elementType) throws
            Exception {
        final EntityTypeImpl<T> et = mock(EntityTypeImpl.class);
        final AbstractPluralAttribute<T, Set, X> att = mock(AbstractPluralAttribute.class);
        when(att.getElementType()).thenReturn(BasicTypeImpl.get(elementType));
        when(att.getCollectionType()).thenReturn(PluralAttribute.CollectionType.SET);
        when(att.getBindableJavaType()).thenReturn(elementType);
        when(att.getJavaField()).thenReturn(entity.getDeclaredField("sources"));
        when(att.getIRI()).thenReturn(IRI.create(Vocabulary.DC_SOURCE));
        return new PluralAnnotationPropertyStrategy<>(et, att, descriptor, mapperMock);
    }

    @OWLClass(iri = Vocabulary.CLASS_BASE)
    private static class WithPluralUriAnnotations {

        @OWLAnnotationProperty(iri = Vocabulary.DC_SOURCE)
        private Set<URI> sources;
    }

    @Test
    void buildAxiomValuesFromInstanceAddNullValueForNullCollection() throws Exception {
        final PluralAnnotationPropertyStrategy<OWLClassN> sut = strategyForN();
        final OWLClassN instance = new OWLClassN();
        sut.buildAxiomValuesFromInstance(instance, gatherer);

        verifyValuesForNPluralAnnotation(Collections.singleton(Value.nullValue()));
    }

    private void verifyValuesForNPluralAnnotation(Collection<Value<?>> expected) throws Exception {
        final AxiomValueDescriptor valueDescriptor = OOMTestUtils.getAxiomValueDescriptor(gatherer);
        assertEquals(Collections.singleton(createAnnotationAssertionForN()), valueDescriptor.getAssertions());
        assertEquals(expected.size(), valueDescriptor.getAssertionValues(createAnnotationAssertionForN()).size());
        assertTrue(valueDescriptor.getAssertionValues(createAnnotationAssertionForN()).containsAll(expected));
    }

    @Test
    void buildAxiomValuesFromInstanceAddsNullValueForEmptyCollection() throws Exception {
        final PluralAnnotationPropertyStrategy<OWLClassN> sut = strategyForN();
        final OWLClassN instance = new OWLClassN();
        instance.setPluralAnnotation(Collections.emptySet());
        sut.buildAxiomValuesFromInstance(instance, gatherer);

        verifyValuesForNPluralAnnotation(Collections.singleton(Value.nullValue()));
    }

    @Test
    void buildAxiomValuesFromInstanceAddsStringValues() throws Exception {
        final PluralAnnotationPropertyStrategy<OWLClassN> sut = strategyForN();
        final OWLClassN instance = new OWLClassN();
        final Set<String> values = IntStream.range(0, 5).mapToObj(i -> "Value" + i).collect(Collectors.toSet());
        instance.setPluralAnnotation(values);
        sut.buildAxiomValuesFromInstance(instance, gatherer);

        verifyValuesForNPluralAnnotation(values.stream().map(Value::new).collect(Collectors.toSet()));
    }

    @Test
    void buildAxiomValuesFromInstanceAddsUrisAsNamedResources() throws Exception {
        final PluralAnnotationPropertyStrategy<WithPluralUriAnnotations> sut = createStrategyWithPluralAnnotations(
                WithPluralUriAnnotations.class, URI.class);
        final Set<URI> values = IntStream.range(0, 5).mapToObj(i -> Generators.createIndividualIdentifier()).collect(
                Collectors.toSet());
        final WithPluralUriAnnotations instance = new WithPluralUriAnnotations();
        instance.sources = values;
        sut.buildAxiomValuesFromInstance(instance, gatherer);

        verifyValuesForNPluralAnnotation(
                values.stream().map(v -> new Value<>(NamedResource.create(v))).collect(Collectors.toSet()));
    }

    @Test
    void addValueFromAxiomTransformsValueToLexicalForm() throws Exception {
        final PluralAnnotationPropertyStrategy<WithPluralStringAnnotations> sut = createStrategyWithPluralAnnotations(
                WithPluralStringAnnotations.class, String.class);
        when(sut.attribute.getConverter()).thenReturn(new ToLexicalFormConverter());
        final Integer value = 117;
        final Axiom<Integer> axiom = new AxiomImpl<>(INDIVIDUAL, Assertion.createAnnotationPropertyAssertion(URI.create(
                DC.Terms.SOURCE), false), new Value<>(value));
        sut.addValueFromAxiom(axiom);
        final WithPluralStringAnnotations instance = new WithPluralStringAnnotations();
        sut.buildInstanceFieldValue(instance);
        assertEquals(Collections.singleton(value.toString()), instance.sources);
    }

    @OWLClass(iri = Vocabulary.CLASS_BASE)
    private static class WithPluralStringAnnotations {

        @OWLAnnotationProperty(iri = Vocabulary.DC_SOURCE)
        private Set<String> sources;
    }

    @Test
    void addValueFromAxiomAcceptsIdentifiersForLexicalFormAttribute() throws Exception {
        final PluralAnnotationPropertyStrategy<WithPluralStringAnnotations> sut = createStrategyWithPluralAnnotations(
                WithPluralStringAnnotations.class, String.class);
        when(sut.attribute.getConverter()).thenReturn(new ToLexicalFormConverter());
        final URI value = Generators.createIndividualIdentifier();
        final Axiom<NamedResource> axiom = new AxiomImpl<>(INDIVIDUAL,
                Assertion.createAnnotationPropertyAssertion(URI.create(
                        DC.Terms.SOURCE), false), new Value<>(NamedResource.create(value)));
        sut.addValueFromAxiom(axiom);
        final WithPluralStringAnnotations instance = new WithPluralStringAnnotations();
        sut.buildInstanceFieldValue(instance);
        assertEquals(Collections.singleton(value.toString()), instance.sources);
    }
}
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

import cz.cvut.kbss.jopa.environment.OWLClassN;
import cz.cvut.kbss.jopa.environment.Vocabulary;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.model.IRI;
import cz.cvut.kbss.jopa.model.JOPAPersistenceProperties;
import cz.cvut.kbss.jopa.model.MultilingualString;
import cz.cvut.kbss.jopa.model.annotations.OWLAnnotationProperty;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.AbstractPluralAttribute;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.BasicTypeImpl;
import cz.cvut.kbss.jopa.model.metamodel.CollectionType;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.IdentifiableEntityType;
import cz.cvut.kbss.jopa.model.metamodel.SetAttributeImpl;
import cz.cvut.kbss.jopa.oom.converter.ConverterWrapper;
import cz.cvut.kbss.jopa.oom.converter.ObjectConverter;
import cz.cvut.kbss.jopa.oom.converter.ToLexicalFormConverter;
import cz.cvut.kbss.jopa.utils.Configuration;
import cz.cvut.kbss.jopa.vocabulary.DC;
import cz.cvut.kbss.ontodriver.descriptor.AxiomValueDescriptor;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.AxiomImpl;
import cz.cvut.kbss.ontodriver.model.LangString;
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
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static org.hamcrest.CoreMatchers.hasItem;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class PluralAnnotationPropertyStrategyTest {

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
        when(mapperMock.getEntityType(OWLClassN.class)).thenReturn(mocks.forOwlClassN().entityType());
    }

    @Test
    void createAssertionCreatesAnnotationPropertyAssertion() {
        final PluralAnnotationPropertyStrategy<OWLClassN> sut = strategyForN();
        final Assertion result = sut.createAssertion();
        assertEquals(Assertion.AssertionType.ANNOTATION_PROPERTY, result.getType());
        assertEquals(DC.Terms.SOURCE, result.getIdentifier().toString());
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
        sut.addAxiomValue(axiom);

        final OWLClassN instance = new OWLClassN();
        sut.buildInstanceFieldValue(instance);
        assertEquals(Collections.singleton(value), instance.getPluralAnnotation());
    }

    private Assertion createAnnotationAssertionForN() {
        return Assertion.createAnnotationPropertyAssertion(URI.create(DC.Terms.SOURCE), LANG, false);
    }

    @Test
    void addValueFromAxiomAddsNamedResourceToValues() {
        final PluralAnnotationPropertyStrategy<OWLClassN> sut = strategyForN();
        final NamedResource value = NamedResource.create(Generators.createIndividualIdentifier());
        final Axiom<NamedResource> axiom =
                new AxiomImpl<>(INDIVIDUAL, createAnnotationAssertionForN(), new Value<>(value));
        sut.addAxiomValue(axiom);

        final OWLClassN instance = new OWLClassN();
        sut.buildInstanceFieldValue(instance);
        assertEquals(Collections.singleton(value.getIdentifier().toString()), instance.getPluralAnnotation());
    }

    @Test
    void addValueFromAxiomAddsStringToUriValues() throws Exception {
        final PluralAnnotationPropertyStrategy<WithPluralUriAnnotations> sut = createStrategyWithPluralAnnotations(
                WithPluralUriAnnotations.class, URI.class, null);
        final NamedResource value = NamedResource.create(Generators.createIndividualIdentifier());
        final Axiom<NamedResource> axiom =
                new AxiomImpl<>(INDIVIDUAL, createAnnotationAssertionForN(), new Value<>(value));
        sut.addAxiomValue(axiom);

        final WithPluralUriAnnotations instance = new WithPluralUriAnnotations();
        sut.buildInstanceFieldValue(instance);
        assertEquals(Collections.singleton(value.getIdentifier()), instance.sources);
    }

    private <T, X> PluralAnnotationPropertyStrategy<T> createStrategyWithPluralAnnotations(Class<T> entity,
                                                                                           Class<X> elementType,
                                                                                           ConverterWrapper converter)
            throws
            Exception {
        final IdentifiableEntityType<T> et = mock(IdentifiableEntityType.class);
        final AbstractPluralAttribute<T, Set, X> att = mock(AbstractPluralAttribute.class);
        when(att.getElementType()).thenReturn(BasicTypeImpl.get(elementType));
        when(att.getCollectionType()).thenReturn(CollectionType.SET);
        when(att.getBindableJavaType()).thenReturn(elementType);
        when(att.getJavaField()).thenReturn(entity.getDeclaredField("sources"));
        when(att.getJavaType()).thenReturn(Set.class);
        when(att.getIRI()).thenReturn(IRI.create(DC.Terms.SOURCE));
        when(att.getConverter()).thenReturn(converter);
        when(att.hasLanguage()).thenReturn(true);
        when(att.getLanguage()).thenReturn(LANG);
        return new PluralAnnotationPropertyStrategy<>(et, att, descriptor, mapperMock);
    }

    @OWLClass(iri = Vocabulary.CLASS_BASE)
    private static class WithPluralUriAnnotations {

        @OWLAnnotationProperty(iri = DC.Terms.SOURCE)
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
                WithPluralUriAnnotations.class, URI.class, null);
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
                WithPluralStringAnnotations.class, String.class, new ToLexicalFormConverter());
        final Integer value = 117;
        final Axiom<Integer> axiom = new AxiomImpl<>(INDIVIDUAL, Assertion.createAnnotationPropertyAssertion(URI.create(
                DC.Terms.SOURCE), false), new Value<>(value));
        sut.addAxiomValue(axiom);
        final WithPluralStringAnnotations instance = new WithPluralStringAnnotations();
        sut.buildInstanceFieldValue(instance);
        assertEquals(Collections.singleton(value.toString()), instance.sources);
    }

    @SuppressWarnings("unused")
    @OWLClass(iri = Vocabulary.CLASS_BASE)
    private static class WithPluralStringAnnotations {

        @OWLAnnotationProperty(iri = DC.Terms.SOURCE)
        private Set<String> sources;
    }

    @Test
    void addValueFromAxiomAcceptsIdentifiersForLexicalFormAttribute() throws Exception {
        final PluralAnnotationPropertyStrategy<WithPluralStringAnnotations> sut = createStrategyWithPluralAnnotations(
                WithPluralStringAnnotations.class, String.class, null);
        when(sut.attribute.getConverter()).thenReturn(new ToLexicalFormConverter());
        final URI value = Generators.createIndividualIdentifier();
        final Axiom<NamedResource> axiom = new AxiomImpl<>(INDIVIDUAL,
                Assertion.createAnnotationPropertyAssertion(URI.create(
                        DC.Terms.SOURCE), false), new Value<>(NamedResource.create(value)));
        sut.addAxiomValue(axiom);
        final WithPluralStringAnnotations instance = new WithPluralStringAnnotations();
        sut.buildInstanceFieldValue(instance);
        assertEquals(Collections.singleton(value.toString()), instance.sources);
    }

    @Test
    void addAxiomValueConvertsNamedResourceToUriForAttributeOfTypeObject() throws Exception {
        final EntityType<ClassWithObjectAnnotation> et = mock(EntityType.class);
        final SetAttributeImpl<ClassWithObjectAnnotation, Object> att = objectAnnotationAttribute(et);
        final PluralAnnotationPropertyStrategy<ClassWithObjectAnnotation> sut = new PluralAnnotationPropertyStrategy<>(
                et, att, descriptor, mapperMock);
        final URI identifier = Generators.createIndividualIdentifier();
        final Axiom<NamedResource> axiom = new AxiomImpl<>(NamedResource.create(PK), createAnnotationAssertionForN(),
                new Value<>(NamedResource.create(identifier)));
        sut.addAxiomValue(axiom);
        final ClassWithObjectAnnotation instance = new ClassWithObjectAnnotation();
        sut.buildInstanceFieldValue(instance);
        assertEquals(Collections.singleton(identifier), instance.pluralAnnotation);
    }

    private SetAttributeImpl<ClassWithObjectAnnotation, Object> objectAnnotationAttribute(
            EntityType<ClassWithObjectAnnotation> et) throws NoSuchFieldException {
        final SetAttributeImpl<ClassWithObjectAnnotation, Object> att = mock(SetAttributeImpl.class);
        when(att.getJavaField()).thenReturn(ClassWithObjectAnnotation.class.getDeclaredField("pluralAnnotation"));
        when(att.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.ANNOTATION);
        when(att.getElementType()).thenReturn(BasicTypeImpl.get(Object.class));
        when(att.getDeclaringType()).thenReturn(et);
        when(att.getCollectionType()).thenReturn(CollectionType.SET);
        when(att.getName())
                .thenReturn(ClassWithObjectAnnotation.class.getDeclaredField("pluralAnnotation").getName());
        when(att.getConverter()).thenReturn(new ObjectConverter());
        when(att.getIRI()).thenReturn(IRI.create(Vocabulary.ATTRIBUTE_BASE + "pluralAnnotation"));
        return att;
    }

    private static class ClassWithObjectAnnotation {
        @OWLAnnotationProperty(iri = Vocabulary.ATTRIBUTE_BASE + "pluralAnnotation")
        private Set<Object> pluralAnnotation;
    }

    @Test
    void buildAxiomValuesFromInstanceConvertsUriToNamedResourceForAttributeOfTypeObject() throws Exception {
        final EntityType<ClassWithObjectAnnotation> et = mock(EntityType.class);
        final SetAttributeImpl<ClassWithObjectAnnotation, Object> att = objectAnnotationAttribute(et);
        final PluralAnnotationPropertyStrategy<ClassWithObjectAnnotation> sut = new PluralAnnotationPropertyStrategy<>(
                et, att, descriptor, mapperMock);
        final ClassWithObjectAnnotation instance = new ClassWithObjectAnnotation();
        instance.pluralAnnotation = Collections.singleton(Generators.createIndividualIdentifier());

        final AxiomValueGatherer builder = new AxiomValueGatherer(NamedResource.create(PK), null);
        sut.buildAxiomValuesFromInstance(instance, builder);
        final AxiomValueDescriptor valueDescriptor = OOMTestUtils.getAxiomValueDescriptor(builder);
        assertEquals(1, valueDescriptor.getAssertions().size());
        final List<Value<?>> values = valueDescriptor
                .getAssertionValues(valueDescriptor.getAssertions().iterator().next());
        assertEquals(1, values.size());
        assertEquals(NamedResource.create((URI) instance.pluralAnnotation.iterator().next()), values.get(0).getValue());
    }

    @Test
    void buildAxiomValuesFromInstanceConvertsMultilingualStringTranslationsToLangStringValues() throws Exception {
        final EntityType<ClassWithObjectAnnotation> et = mock(EntityType.class);
        final SetAttributeImpl<ClassWithObjectAnnotation, Object> att = objectAnnotationAttribute(et);
        final PluralAnnotationPropertyStrategy<ClassWithObjectAnnotation> sut = new PluralAnnotationPropertyStrategy<>(
                et, att, descriptor, mapperMock);
        final ClassWithObjectAnnotation instance = new ClassWithObjectAnnotation();
        final MultilingualString mls = MultilingualString.create("test", "en");
        mls.set("cs", "test");
        final MultilingualString mlsTwo = MultilingualString.create("building", "en");
        instance.pluralAnnotation = new HashSet<>(Arrays.asList(mls, mlsTwo));
        final AxiomValueGatherer builder = new AxiomValueGatherer(NamedResource.create(PK), null);
        sut.buildAxiomValuesFromInstance(instance, builder);
        final AxiomValueDescriptor valueDescriptor = OOMTestUtils.getAxiomValueDescriptor(builder);
        assertEquals(1, valueDescriptor.getAssertions().size());
        final List<Value<?>> values = valueDescriptor
                .getAssertionValues(valueDescriptor.getAssertions().iterator().next());
        mls.getValue().forEach((lang, value) -> assertThat(values, hasItem(new Value<>(new LangString(value, lang)))));
        mlsTwo.getValue()
              .forEach((lang, value) -> assertThat(values, hasItem(new Value<>(new LangString(value, lang)))));
    }
}

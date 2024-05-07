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
import cz.cvut.kbss.jopa.environment.OWLClassB;
import cz.cvut.kbss.jopa.environment.OWLClassD;
import cz.cvut.kbss.jopa.environment.OWLClassM;
import cz.cvut.kbss.jopa.environment.OWLClassQ;
import cz.cvut.kbss.jopa.environment.OWLClassU;
import cz.cvut.kbss.jopa.environment.Vocabulary;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.model.IRI;
import cz.cvut.kbss.jopa.model.annotations.FetchType;
import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.OWLAnnotationProperty;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;
import cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty;
import cz.cvut.kbss.jopa.model.annotations.Sequence;
import cz.cvut.kbss.jopa.model.annotations.SequenceType;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.Attribute.PersistentAttributeType;
import cz.cvut.kbss.jopa.model.metamodel.CollectionType;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.ListAttribute;
import cz.cvut.kbss.jopa.sessions.util.LoadingParameters;
import cz.cvut.kbss.jopa.vocabulary.RDF;
import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.net.URI;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.empty;
import static org.hamcrest.Matchers.hasItems;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class AxiomDescriptorFactoryTest {

    private static final URI CONTEXT = URI.create("http://krizik.felk.cvut.cz/ontologies/contextOne");
    private static final URI ID = URI.create(Vocabulary.INDIVIDUAL_BASE + "X");

    private static URI stringAttAUri;
    private static URI stringAttBUri;
    private static URI owlClassAAttUri;

    private MetamodelMocks metamodelMocks;

    private Descriptor descriptor;
    private Descriptor descriptorInContext;

    private AxiomDescriptorFactory sut;

    @BeforeAll
    static void setUpBeforeClass() throws Exception {
        stringAttAUri = URI.create(OWLClassA.getStrAttField().getAnnotation(OWLDataProperty.class).iri());
        stringAttBUri = URI.create(OWLClassB.getStrAttField().getAnnotation(OWLDataProperty.class).iri());
        owlClassAAttUri = URI.create(OWLClassD.getOwlClassAField().getAnnotation(OWLObjectProperty.class).iri());
    }

    @BeforeEach
    void setUp() throws Exception {
        this.metamodelMocks = new MetamodelMocks();
        this.descriptor = new EntityDescriptor();
        this.descriptorInContext = new EntityDescriptor(CONTEXT);

        sut = new AxiomDescriptorFactory();
    }

    @Test
    void testCreateForEntityLoadingWithTypes() {
        final AxiomDescriptor res = sut
                .createForEntityLoading(new LoadingParameters<>(OWLClassA.class, ID, descriptor),
                        metamodelMocks.forOwlClassA().entityType());
        // Types specification and the string attribute
        assertEquals(2, res.getAssertions().size());
        assertEquals(NamedResource.create(ID), res.getSubject());
        assertThat(res.getSubjectContexts(), empty());
        assertTrue(res.getAssertions()
                      .contains(Assertion.createDataPropertyAssertion(stringAttAUri, Generators.LANG, false)));
        assertTrue(res.getAssertions().contains(Assertion.createClassAssertion(false)));
    }

    @Test
    void testCreateForEntityLoadingWithTypesInContext() {
        descriptor.addAttributeContext(metamodelMocks.forOwlClassA().typesSpec(), CONTEXT);
        final AxiomDescriptor res = sut
                .createForEntityLoading(new LoadingParameters<>(OWLClassA.class, ID, descriptor),
                        metamodelMocks.forOwlClassA().entityType());
        // Types specification and the string attribute
        assertEquals(2, res.getAssertions().size());
        assertEquals(NamedResource.create(ID), res.getSubject());
        assertThat(res.getSubjectContexts(), empty());
        assertTrue(res.getAssertions().contains(
                Assertion.createDataPropertyAssertion(stringAttAUri, Generators.LANG, false)));
        assertTrue(res.getAssertions().contains(Assertion.createClassAssertion(false)));
        assertEquals(Collections.singleton(CONTEXT), res.getAssertionContexts(Assertion.createClassAssertion(false)));
    }

    @Test
    void testCreateForEntityLoadingWithPropertiesAndContext() {
        final AxiomDescriptor res = sut
                .createForEntityLoading(new LoadingParameters<>(OWLClassB.class, ID, descriptorInContext),
                        metamodelMocks.forOwlClassB().entityType());
        // Class assertion, properties specification and the string attribute
        assertEquals(3, res.getAssertions().size());
        assertEquals(NamedResource.create(ID), res.getSubject());
        assertEquals(Collections.singleton(CONTEXT), res.getSubjectContexts());
        assertTrue(res.getAssertions()
                      .contains(Assertion.createDataPropertyAssertion(stringAttBUri, Generators.LANG, false)));
    }

    @Test
    void testCreateForEntityLoadingWithObjectPropertyInContext() {
        descriptor.addAttributeContext(metamodelMocks.forOwlClassA().stringAttribute(), CONTEXT);
        final AxiomDescriptor res = sut
                .createForEntityLoading(new LoadingParameters<>(OWLClassA.class, ID, descriptor),
                        metamodelMocks.forOwlClassA().entityType());
        // Class assertion and the object property assertion
        assertEquals(2, res.getAssertions().size());
        assertEquals(NamedResource.create(ID), res.getSubject());
        assertThat(res.getSubjectContexts(), empty());
        final Optional<Assertion> ass = res.getAssertions().stream()
                                           .filter(a -> a.getIdentifier().toString()
                                                         .equals(Vocabulary.p_a_stringAttribute)).findAny();
        assertTrue(ass.isPresent());
        assertEquals(Collections.singleton(CONTEXT), res.getAssertionContexts(ass.get()));
        assertEquals(Vocabulary.p_a_stringAttribute, ass.get().getIdentifier().toString());
    }

    @Test
    void testCreateForEntityLoadingWithAnnotationProperty() {
        // Artificially change the attribute type to annotation
        when(metamodelMocks.forOwlClassD().owlClassAAtt().getPersistentAttributeType()).thenReturn(
                PersistentAttributeType.ANNOTATION);
        final AxiomDescriptor res = sut
                .createForEntityLoading(new LoadingParameters<>(OWLClassD.class, ID, descriptor),
                        metamodelMocks.forOwlClassD().entityType());
        // Class assertion and the annotation property assertion
        assertEquals(2, res.getAssertions().size());
        assertEquals(NamedResource.create(ID), res.getSubject());
        assertThat(res.getSubjectContexts(), empty());
        assertTrue(res.getAssertions().contains(
                Assertion.createAnnotationPropertyAssertion(owlClassAAttUri, false)));
    }

    @Test
    void createForEntityLoadingWithLazilyLoadedAttribute() {
        when(metamodelMocks.forOwlClassA().stringAttribute().getFetchType()).thenReturn(FetchType.LAZY);
        final AxiomDescriptor res = sut
                .createForEntityLoading(new LoadingParameters<>(OWLClassA.class, ID, descriptor),
                        metamodelMocks.forOwlClassA().entityType());
        // Types specification (class assertion)
        assertEquals(1, res.getAssertions().size());
        assertEquals(NamedResource.create(ID), res.getSubject());
        assertThat(res.getSubjectContexts(), empty());
        assertFalse(res.getAssertions().contains(
                Assertion.createDataPropertyAssertion(stringAttAUri, false)));
        assertTrue(res.getAssertions().contains(Assertion.createClassAssertion(false)));
    }

    @Test
    void testCreateForFieldLoadingDataProperty() {
        final Descriptor desc = new EntityDescriptor();
        when(metamodelMocks.forOwlClassA().stringAttribute().getFetchType()).thenReturn(FetchType.LAZY);
        final AxiomDescriptor res = sut.createForFieldLoading(ID, metamodelMocks.forOwlClassA().stringAttribute(),
                desc, metamodelMocks.forOwlClassA().entityType());
        assertNotNull(res);
        assertEquals(1, res.getAssertions().size());
        assertTrue(res.getAssertions().contains(
                Assertion.createDataPropertyAssertion(stringAttAUri, Generators.LANG, false)));
    }

    @Test
    void testCreateForFieldLoadingObjectPropertyInEntityContext() {
        final Descriptor desc = new EntityDescriptor(false);
        desc.addAttributeDescriptor(metamodelMocks.forOwlClassD().owlClassAAtt(), new EntityDescriptor(CONTEXT));
        final AxiomDescriptor res = sut.createForFieldLoading(ID,
                metamodelMocks.forOwlClassD().owlClassAAtt(), desc,
                metamodelMocks.forOwlClassD().entityType());
        assertEquals(1, res.getAssertions().size());
        final Assertion as = res.getAssertions().iterator().next();
        assertEquals(Assertion.createObjectPropertyAssertion(owlClassAAttUri, false), as);
        assertEquals(Collections.singleton(CONTEXT), res.getAssertionContexts(as));
    }

    @Test
    void testCreateForFieldLoadingTypes() {
        final Descriptor desc = new EntityDescriptor(CONTEXT);
        final AxiomDescriptor res = sut.createForFieldLoading(ID, metamodelMocks.forOwlClassA().typesSpec(),
                desc, metamodelMocks.forOwlClassA().entityType());
        assertEquals(1, res.getAssertions().size());
        final Assertion as = res.getAssertions().iterator().next();
        assertEquals(Assertion.createClassAssertion(metamodelMocks.forOwlClassA().typesSpec().isInferred()), as);
        assertEquals(Collections.singleton(CONTEXT), res.getAssertionContexts(as));
    }

    @Test
    void testCreateForFieldLoadingProperties() {
        final Descriptor desc = new EntityDescriptor();
        final AxiomDescriptor res = sut.createForFieldLoading(ID, metamodelMocks.forOwlClassB().propertiesSpec(), desc,
                metamodelMocks.forOwlClassB().entityType());
        assertEquals(1, res.getAssertions().size());
        final Assertion as = res.getAssertions().iterator().next();
        assertEquals(Assertion
                        .createUnspecifiedPropertyAssertion(
                                metamodelMocks.forOwlClassB().propertiesSpec().isInferred()),
                as);
    }

    @Test
    void createForEntityLoadingIncludesMappedSuperclassAttributes() throws Exception {
        final Descriptor desc = new EntityDescriptor();
        final AxiomDescriptor res = sut.createForEntityLoading(loadingParameters(OWLClassQ.class, desc),
                metamodelMocks.forOwlClassQ().entityType());
        final Set<String> propertyIris = OWLClassQ.getPersistentFields().stream().map(f -> {
            if (f.getAnnotation(OWLDataProperty.class) != null) {
                return f.getAnnotation(OWLDataProperty.class).iri();
            } else if (f.getAnnotation(OWLObjectProperty.class) != null) {
                return f.getAnnotation(OWLObjectProperty.class).iri();
            } else {
                return f.getAnnotation(OWLAnnotationProperty.class).iri();
            }
        }).collect(Collectors.toSet());
        final Set<Assertion> assertions = res.getAssertions();
        // + class assertion
        assertEquals(OWLClassQ.getPersistentFields().size() + 1, assertions.size());
        for (Assertion a : assertions) {
            if (a.getType() != Assertion.AssertionType.CLASS) {
                assertTrue(propertyIris.contains(a.getIdentifier().toString()));
            }
        }
    }

    private <T> LoadingParameters<T> loadingParameters(Class<T> cls, Descriptor descriptor) {
        return new LoadingParameters<>(cls, ID, descriptor);
    }

    @Test
    void createForEntityLoadingSetsLanguageTagAccordingToDescriptor() {
        final Descriptor descriptor = new EntityDescriptor();
        descriptor.setLanguage("en");
        final AxiomDescriptor res = sut.createForEntityLoading(loadingParameters(OWLClassA.class, descriptor),
                metamodelMocks.forOwlClassA().entityType());
        final Set<Assertion> assertions = res.getAssertions();
        assertions.stream().filter(a -> a.getType() != Assertion.AssertionType.CLASS && a.getType() !=
                Assertion.AssertionType.OBJECT_PROPERTY).forEach(a -> assertEquals("en", a.getLanguage()));
    }

    @Test
    void createForEntityLoadingSetsLanguageTagOfSpecificAssertionAccordingToDescriptor() {
        final Descriptor descriptor = new EntityDescriptor();
        descriptor.setLanguage("en");
        descriptor.setAttributeLanguage(metamodelMocks.forOwlClassA().stringAttribute(), "cs");
        final AxiomDescriptor res = sut.createForEntityLoading(loadingParameters(OWLClassA.class, descriptor),
                metamodelMocks.forOwlClassA().entityType());
        final Set<Assertion> assertions = res.getAssertions();
        final Optional<Assertion> strAssertion = assertions.stream().filter(a -> a.getIdentifier().equals(URI
                                                                   .create(Vocabulary.p_a_stringAttribute)))
                                                           .findAny();
        assertTrue(strAssertion.isPresent());
        assertEquals("cs", strAssertion.get().getLanguage());
    }

    @Test
    void createForEntityLoadingSetsLanguageTagAccordingToGlobalPUSpecification() {
        this.sut = new AxiomDescriptorFactory();
        final AxiomDescriptor res = sut.createForEntityLoading(loadingParameters(OWLClassA.class, descriptor),
                metamodelMocks.forOwlClassA().entityType());
        final Set<Assertion> assertions = res.getAssertions();
        assertions.stream().filter(a -> a.getType() != Assertion.AssertionType.CLASS && a.getType() !=
                Assertion.AssertionType.OBJECT_PROPERTY).forEach(a -> assertEquals(Generators.LANG, a.getLanguage()));
    }

    @Test
    void createForFieldLoadingSetsLanguageTagBasedOnDescriptorLanguageTag() {
        final Descriptor descriptor = new EntityDescriptor();
        descriptor.setLanguage("en");
        final AxiomDescriptor res =
                sut.createForFieldLoading(ID, metamodelMocks.forOwlClassA().stringAttribute(), descriptor,
                        metamodelMocks.forOwlClassA().entityType());
        final Set<Assertion> assertions = res.getAssertions();
        assertEquals(1, assertions.size());
        assertEquals("en", assertions.iterator().next().getLanguage());
    }

    @Test
    void createForFieldLoadingSetsLanguageTagBasedOnAttributeLanguageTagInDescriptor() {
        final Descriptor descriptor = new EntityDescriptor();
        descriptor.setAttributeLanguage(metamodelMocks.forOwlClassA().stringAttribute(), "cs");
        final AxiomDescriptor res =
                sut.createForFieldLoading(ID, metamodelMocks.forOwlClassA().stringAttribute(), descriptor,
                        metamodelMocks.forOwlClassA().entityType());
        final Set<Assertion> assertions = res.getAssertions();
        assertEquals(1, assertions.size());
        assertEquals("cs", assertions.iterator().next().getLanguage());
    }

    @Test
    void createForFieldLoadingSetsLanguageOfPUWhenDescriptorLanguageIsNotSpecified() {
        this.sut = new AxiomDescriptorFactory();
        final AxiomDescriptor res =
                sut.createForFieldLoading(ID, metamodelMocks.forOwlClassA().stringAttribute(), descriptor,
                        metamodelMocks.forOwlClassA().entityType());
        final Set<Assertion> assertions = res.getAssertions();
        assertEquals(1, assertions.size());
        assertEquals(Generators.LANG, assertions.iterator().next().getLanguage());
    }

    @Test
    void createForEntityLoadingAllowsOverridingPULevelLanguageSetting() {
        descriptor.setLanguage(null);
        final AxiomDescriptor res = sut.createForEntityLoading(loadingParameters(OWLClassA.class, descriptor),
                metamodelMocks.forOwlClassA().entityType());
        final Set<Assertion> assertions = res.getAssertions();
        assertions.stream().filter(a -> a.getType() != Assertion.AssertionType.CLASS && a.getType() !=
                Assertion.AssertionType.OBJECT_PROPERTY).forEach(a -> {
            assertFalse(a.hasLanguage());
            assertNull(a.getLanguage());
        });
    }

    @Test
    void createForFieldLoadingAllowsOverridingPULevelLanguageSetting() {
        final Descriptor descriptor = new EntityDescriptor();
        descriptor.setAttributeLanguage(metamodelMocks.forOwlClassA().stringAttribute(), null);
        final AxiomDescriptor res =
                sut.createForFieldLoading(ID, metamodelMocks.forOwlClassA().stringAttribute(), descriptor,
                        metamodelMocks.forOwlClassA().entityType());
        final Set<Assertion> assertions = res.getAssertions();
        assertEquals(1, assertions.size());
        assertFalse(assertions.iterator().next().hasLanguage());
        assertNull(assertions.iterator().next().getLanguage());
    }

    @Test
    void createForLoadingCreatesDataPropertyAssertionWithoutLanguageWhenNoneIsSetForAttributeAndInDescriptor() {
        final Descriptor descriptor = new EntityDescriptor();
        when(metamodelMocks.forOwlClassA().stringAttribute().hasLanguage()).thenReturn(false);
        final AxiomDescriptor res =
                sut.createForFieldLoading(ID, metamodelMocks.forOwlClassA().stringAttribute(), descriptor,
                        metamodelMocks.forOwlClassA().entityType());
        final Set<Assertion> assertions = res.getAssertions();
        assertEquals(1, assertions.size());
        assertFalse(assertions.iterator().next().hasLanguage());
    }

    @Test
    void createForLoadingCreatesAnnotationPropertyAssertionWithoutLanguageWhenNoneIsSetForAttributeAndInDescriptor() {
        final Descriptor descriptor = new EntityDescriptor();
        when(metamodelMocks.forOwlClassN().annotationAttribute().hasLanguage()).thenReturn(false);
        final AxiomDescriptor res = sut
                .createForFieldLoading(ID, metamodelMocks.forOwlClassN().annotationAttribute(), descriptor,
                        metamodelMocks.forOwlClassN().entityType());
        final Set<Assertion> assertions = res.getAssertions();
        assertEquals(1, assertions.size());
        assertFalse(assertions.iterator().next().hasLanguage());
    }

    @Test
    void createForReferenceLoadingCreatesClassAssertionAxiom() {
        final LoadingParameters<OWLClassA> params = new LoadingParameters<>(OWLClassA.class, ID, descriptor);
        final Axiom<NamedResource> result =
                sut.createForReferenceLoading(params.getIdentifier(), metamodelMocks.forOwlClassA().entityType());
        assertNotNull(result);
        assertEquals(Assertion.createClassAssertion(false), result.getAssertion());
        assertEquals(ID, result.getSubject().getIdentifier());
        assertEquals(Vocabulary.c_OwlClassA, result.getValue().stringValue());
    }

    @Test
    void createForEntityLoadingUsesSubjectContextForAssertionWhenAssertionsInSubjectContextIsConfiguredInDescriptor() {
        descriptor.addAttributeDescriptor(metamodelMocks.forOwlClassD().owlClassAAtt(), descriptorInContext);
        final LoadingParameters<OWLClassD> params = new LoadingParameters<>(OWLClassD.class, ID, descriptor);
        final AxiomDescriptor desc = sut.createForEntityLoading(params, metamodelMocks.forOwlClassD().entityType());
        assertEquals(Collections.emptySet(), desc.getAssertionContexts(
                Assertion.createObjectPropertyAssertion(URI.create(Vocabulary.P_HAS_A), false)));
    }

    @Test
    void createForFieldLoadingUsesSubjectContextForAssertionWhenAssertionsInSubjectContextIsConfiguredInDescriptor() {
        descriptor.addAttributeDescriptor(metamodelMocks.forOwlClassD().owlClassAAtt(), descriptorInContext);
        final AxiomDescriptor desc =
                sut.createForFieldLoading(ID, metamodelMocks.forOwlClassD().owlClassAAtt(), descriptor,
                        metamodelMocks.forOwlClassD().entityType());
        assertEquals(Collections.emptySet(), desc.getAssertionContexts(
                Assertion.createObjectPropertyAssertion(URI.create(Vocabulary.P_HAS_A), false)));
    }

    @Test
    void createForEntityLoadingAddsAssertionWithoutLanguageTagForSimpleLiteralAttribute() {
        final LoadingParameters<OWLClassM> params = new LoadingParameters<>(OWLClassM.class, ID, descriptor);
        final AxiomDescriptor desc = sut
                .createForEntityLoading(params, metamodelMocks.forOwlClassM().entityType());
        final Optional<Assertion> result = desc.getAssertions().stream().filter(a -> a.getIdentifier()
                                                                                      .equals(URI.create(
                                                                                              Vocabulary.p_m_simpleLiteral)))
                                               .findAny();
        assertTrue(result.isPresent());
        assertFalse(result.get().hasLanguage());
    }

    @Test
    void createForFieldLoadingAddsAssertionWithoutLanguageTagForSimpleLiteralAttribute() {
        final AxiomDescriptor desc =
                sut.createForFieldLoading(ID, metamodelMocks.forOwlClassM().simpleLiteralAttribute(), descriptor,
                        metamodelMocks.forOwlClassM().entityType());
        assertEquals(1, desc.getAssertions().size());
        final Assertion result = desc.getAssertions().iterator().next();
        assertFalse(result.hasLanguage());
    }

    @Test
    void createForLoadingSetsDefaultContextForAssertionInDefaultWhenSubjectContextIsDifferent() {
        descriptorInContext.addAttributeContext(metamodelMocks.forOwlClassA().stringAttribute(), null);
        final LoadingParameters<OWLClassA> params = new LoadingParameters<>(OWLClassA.class, ID, descriptorInContext);

        final AxiomDescriptor desc = sut
                .createForEntityLoading(params, metamodelMocks.forOwlClassA().entityType());
        assertEquals(Collections.singleton(CONTEXT), desc.getSubjectContexts());
        final Set<URI> result = desc.getAssertionContexts(
                Assertion.createDataPropertyAssertion(URI.create(Vocabulary.p_a_stringAttribute), Generators.LANG,
                        false));
        assertNotNull(result);
        assertTrue(result.isEmpty());
    }

    @Test
    void createForEntityLoadingAddsAssertionsWithoutLanguageTagForMultilingualAttributes() {
        final LoadingParameters<OWLClassU> params = new LoadingParameters<>(OWLClassU.class, ID, descriptor);
        final AxiomDescriptor desc = sut.createForEntityLoading(params, metamodelMocks.forOwlClassU().entityType());

        final Optional<Assertion> singularAssertion = desc.getAssertions().stream()
                                                          .filter(a -> a.getIdentifier().toString()
                                                                        .equals(Vocabulary.P_U_SINGULAR_MULTILINGUAL_ATTRIBUTE))
                                                          .findAny();
        assertTrue(singularAssertion.isPresent());
        assertFalse(singularAssertion.get().hasLanguage());
        final Optional<Assertion> pluralAssertion = desc.getAssertions().stream()
                                                        .filter(a -> a.getIdentifier().toString()
                                                                      .equals(Vocabulary.P_U_PLURAL_MULTILINGUAL_ATTRIBUTE))
                                                        .findAny();
        assertTrue(pluralAssertion.isPresent());
        assertFalse(pluralAssertion.get().hasLanguage());
    }

    @Test
    void createForEntityLoadingSetsInferredOnAssertionToFalseWhenDescriptorOverridesMetamodelConfiguration() {
        final LoadingParameters<OWLClassD> params = new LoadingParameters<>(OWLClassD.class, ID, descriptor);
        descriptor.disableInference();
        when(metamodelMocks.forOwlClassD().owlClassAAtt().isInferred()).thenReturn(true);

        final AxiomDescriptor desc = sut.createForEntityLoading(params, metamodelMocks.forOwlClassD().entityType());
        final Optional<Assertion> assertion =
                desc.getAssertions().stream().filter(a -> a.getIdentifier().toString().equals(Vocabulary.p_h_hasA))
                    .findAny();
        assertTrue(assertion.isPresent());
        assertFalse(assertion.get().isInferred());
    }

    @Test
    void createForEntityLoadingSetsInferredOnClassAssertionToFalseWhenDescriptorOverridesMetamodelConfiguration() {
        descriptor.disableInference();
        when(metamodelMocks.forOwlClassA().typesSpec().isInferred()).thenReturn(true);
        final LoadingParameters<OWLClassA> params = new LoadingParameters<>(OWLClassA.class, ID, descriptor);
        final AxiomDescriptor desc = sut.createForEntityLoading(params, metamodelMocks.forOwlClassA().entityType());
        final Optional<Assertion> assertion =
                desc.getAssertions().stream().filter(a -> a.getIdentifier().toString().equals(RDF.TYPE))
                    .findAny();
        assertTrue(assertion.isPresent());
        assertFalse(assertion.get().isInferred());
    }

    @Test
    void createForEntityLoadingSetsInferenceOnUnspecifiedPropertyAssertionToFalseWhenDescriptorOverridesMetamodelConfiguration() {
        descriptor.disableInference();
        when(metamodelMocks.forOwlClassB().propertiesSpec().isInferred()).thenReturn(true);
        final LoadingParameters<OWLClassB> params = new LoadingParameters<>(OWLClassB.class, ID, descriptor);
        final AxiomDescriptor desc = sut.createForEntityLoading(params, metamodelMocks.forOwlClassB().entityType());
        final Optional<Assertion> assertion =
                desc.getAssertions().stream().filter(a -> Assertion.createUnspecifiedPropertyAssertion(false).equals(a))
                    .findAny();
        assertTrue(assertion.isPresent());
        assertFalse(assertion.get().isInferred());
    }

    @Test
    void createForFieldLoadingSetsInferredOnAssertionToFalseWhenDescriptorOverridesMetamodelConfiguration() {
        descriptor.disableInference();
        when(metamodelMocks.forOwlClassA().stringAttribute().isInferred()).thenReturn(true);

        final AxiomDescriptor desc =
                sut.createForFieldLoading(ID, metamodelMocks.forOwlClassA().stringAttribute(), descriptor,
                        metamodelMocks.forOwlClassA().entityType());
        final Optional<Assertion> assertion = desc.getAssertions().stream()
                                                  .filter(a -> a.getIdentifier().toString()
                                                                .equals(Vocabulary.p_a_stringAttribute))
                                                  .findAny();
        assertTrue(assertion.isPresent());
        assertFalse(assertion.get().isInferred());
    }

    @Test
    void createForFieldLoadingSetsInferredOnClassAssertionToFalseWhenDescriptorOverridesMetamodelConfiguration() {
        descriptor.disableInference();
        when(metamodelMocks.forOwlClassA().typesSpec().isInferred()).thenReturn(true);

        final AxiomDescriptor desc =
                sut.createForFieldLoading(ID, metamodelMocks.forOwlClassA().typesSpec(), descriptor,
                        metamodelMocks.forOwlClassA().entityType());
        final Optional<Assertion> assertion = desc.getAssertions().stream()
                                                  .filter(a -> a.getIdentifier().toString().equals(RDF.TYPE))
                                                  .findAny();
        assertTrue(assertion.isPresent());
        assertFalse(assertion.get().isInferred());
    }

    @Test
    void createForFieldLoadingSetsInferredOnUnspecifiedPropertyAssertionToFalseWhenDescriptorOverridesMetamodelConfiguration() {
        descriptor.disableInference();
        when(metamodelMocks.forOwlClassB().propertiesSpec().isInferred()).thenReturn(true);

        final AxiomDescriptor desc =
                sut.createForFieldLoading(ID, metamodelMocks.forOwlClassB().propertiesSpec(), descriptor,
                        metamodelMocks.forOwlClassB().entityType());
        final Optional<Assertion> assertion =
                desc.getAssertions().stream().filter(a -> a.equals(Assertion.createUnspecifiedPropertyAssertion(false)))
                    .findAny();
        assertTrue(assertion.isPresent());
        assertFalse(assertion.get().isInferred());
    }

    /**
     * This is because for a referenced list, the nodes themselves are individuals (resources), so an object property is
     * needed. But if the value is a data property (literal), the mapping uses @OWLDataProperty as well
     */
    @Test
    void createForEntityLoadingReturnsObjectPropertyAssertionForReferencedListDataProperty() {
        final LoadingParameters<WithDataPropertyReferencedList> lp = new LoadingParameters<>(WithDataPropertyReferencedList.class, ID, descriptor);
        final EntityType<WithDataPropertyReferencedList> et = mock(EntityType.class);
        final ListAttribute<WithDataPropertyReferencedList, Integer> att = mock(ListAttribute.class);
        when(et.getAttributes()).thenReturn(Set.of(att));
        when(att.isAssociation()).thenReturn(false);
        when(att.getPersistentAttributeType()).thenReturn(PersistentAttributeType.DATA);
        when(att.isCollection()).thenReturn(true);
        when(att.getCollectionType()).thenReturn(CollectionType.LIST);
        when(att.getSequenceType()).thenReturn(SequenceType.referenced);
        when(att.getIRI()).thenReturn(IRI.create(Vocabulary.ATTRIBUTE_BASE + "literalReferencedList"));

        final AxiomDescriptor desc = sut.createForEntityLoading(lp, et);
        final Optional<Assertion> listAss = desc.getAssertions().stream().filter(a -> a.getIdentifier().toString()
                                                                                       .equals(Vocabulary.ATTRIBUTE_BASE + "literalReferencedList"))
                                                .findAny();
        assertTrue(listAss.isPresent());
        assertEquals(Assertion.AssertionType.OBJECT_PROPERTY, listAss.get().getType());
    }

    private static class WithDataPropertyReferencedList {
        @Id
        private URI uri;

        @Sequence(type = SequenceType.referenced)
        @OWLDataProperty(iri = Vocabulary.ATTRIBUTE_BASE + "literalReferencedList")
        private List<Integer> literalReferencedList;
    }

    @Test
    void createForEntityLoadingMergesSubjectContextWithTypesContextsForClassAssertionRetrieval() {
        final URI attContext = Generators.createIndividualIdentifier();
        descriptorInContext.addAttributeContext(metamodelMocks.forOwlClassA().typesSpec(), attContext);
        descriptorInContext.addAttributeContext(metamodelMocks.forOwlClassA().stringAttribute(), attContext);
        final LoadingParameters<OWLClassA> lp = new LoadingParameters<>(OWLClassA.class, ID, descriptorInContext);
        final AxiomDescriptor result = sut.createForEntityLoading(lp, metamodelMocks.forOwlClassA().entityType());
        final Set<URI> contexts = result.getAssertionContexts(Assertion.createClassAssertion(false));
        assertThat(contexts, hasItems(attContext, CONTEXT));
    }
}

/**
 * Copyright (C) 2020 Czech Technical University in Prague
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

import cz.cvut.kbss.jopa.environment.*;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.model.annotations.FetchType;
import cz.cvut.kbss.jopa.model.annotations.OWLAnnotationProperty;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;
import cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.Attribute.PersistentAttributeType;
import cz.cvut.kbss.jopa.sessions.LoadingParameters;
import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.net.URI;
import java.util.Collections;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.empty;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.when;

class AxiomDescriptorFactoryTest {

    private static final URI CONTEXT = URI.create("http://krizik.felk.cvut.cz/ontologies/contextOne");
    private static final URI PK = URI.create("http://krizik.felk.cvut.cz/ontologies/entityX");

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
                .createForEntityLoading(new LoadingParameters<>(OWLClassA.class, PK, descriptor),
                        metamodelMocks.forOwlClassA().entityType());
        // Types specification and the string attribute
        assertEquals(2, res.getAssertions().size());
        assertEquals(NamedResource.create(PK), res.getSubject());
        assertThat(res.getSubjectContexts(), empty());
        assertTrue(res.getAssertions()
                      .contains(Assertion.createDataPropertyAssertion(stringAttAUri, Generators.LANG, false)));
        assertTrue(res.getAssertions().contains(Assertion.createClassAssertion(false)));
    }

    @Test
    void testCreateForEntityLoadingWithTypesInContext() {
        descriptor.addAttributeContext(metamodelMocks.forOwlClassA().typesSpec(), CONTEXT);
        final AxiomDescriptor res = sut
                .createForEntityLoading(new LoadingParameters<>(OWLClassA.class, PK, descriptor),
                        metamodelMocks.forOwlClassA().entityType());
        // Types specification and the string attribute
        assertEquals(2, res.getAssertions().size());
        assertEquals(NamedResource.create(PK), res.getSubject());
        assertThat(res.getSubjectContexts(), empty());
        assertTrue(res.getAssertions().contains(
                Assertion.createDataPropertyAssertion(stringAttAUri, Generators.LANG, false)));
        assertTrue(res.getAssertions().contains(Assertion.createClassAssertion(false)));
        assertEquals(Collections.singleton(CONTEXT), res.getAssertionContexts(Assertion.createClassAssertion(false)));
    }

    @Test
    void testCreateForEntityLoadingWithPropertiesAndContext() {
        final AxiomDescriptor res = sut
                .createForEntityLoading(new LoadingParameters<>(OWLClassB.class, PK, descriptorInContext),
                        metamodelMocks.forOwlClassB().entityType());
        // Class assertion, properties specification and the string attribute
        assertEquals(3, res.getAssertions().size());
        assertEquals(NamedResource.create(PK), res.getSubject());
        assertEquals(Collections.singleton(CONTEXT), res.getSubjectContexts());
        assertTrue(res.getAssertions()
                      .contains(Assertion.createDataPropertyAssertion(stringAttBUri, Generators.LANG, false)));
    }

    @Test
    void testCreateForEntityLoadingWithObjectPropertyInContext() {
        descriptor.addAttributeContext(metamodelMocks.forOwlClassD().owlClassAAtt(), CONTEXT);
        final AxiomDescriptor res = sut
                .createForEntityLoading(new LoadingParameters<>(OWLClassD.class, PK, descriptor),
                        metamodelMocks.forOwlClassD().entityType());
        // Class assertion and the object property assertion
        assertEquals(2, res.getAssertions().size());
        assertEquals(NamedResource.create(PK), res.getSubject());
        assertThat(res.getSubjectContexts(), empty());
        final Optional<Assertion> ass = res.getAssertions().stream()
                                           .filter(a -> a.getIdentifier().equals(owlClassAAttUri)).findAny();
        assertTrue(ass.isPresent());
        assertEquals(Collections.singleton(CONTEXT), res.getAssertionContexts(ass.get()));
        assertEquals(owlClassAAttUri, ass.get().getIdentifier());
    }

    @Test
    void testCreateForEntityLoadingWithAnnotationProperty() {
        // Artificially change the attribute type to annotation
        when(metamodelMocks.forOwlClassD().owlClassAAtt().getPersistentAttributeType()).thenReturn(
                PersistentAttributeType.ANNOTATION);
        final AxiomDescriptor res = sut
                .createForEntityLoading(new LoadingParameters<>(OWLClassD.class, PK, descriptor),
                        metamodelMocks.forOwlClassD().entityType());
        // Class assertion and the annotation property assertion
        assertEquals(2, res.getAssertions().size());
        assertEquals(NamedResource.create(PK), res.getSubject());
        assertThat(res.getSubjectContexts(), empty());
        assertTrue(res.getAssertions().contains(
                Assertion.createAnnotationPropertyAssertion(owlClassAAttUri, false)));
    }

    @Test
    void createForEntityLoadingWithLazilyLoadedAttribute() {
        when(metamodelMocks.forOwlClassA().stringAttribute().getFetchType()).thenReturn(FetchType.LAZY);
        final AxiomDescriptor res = sut
                .createForEntityLoading(new LoadingParameters<>(OWLClassA.class, PK, descriptor),
                        metamodelMocks.forOwlClassA().entityType());
        // Types specification (class assertion)
        assertEquals(1, res.getAssertions().size());
        assertEquals(NamedResource.create(PK), res.getSubject());
        assertThat(res.getSubjectContexts(), empty());
        assertFalse(res.getAssertions().contains(
                Assertion.createDataPropertyAssertion(stringAttAUri, false)));
        assertTrue(res.getAssertions().contains(Assertion.createClassAssertion(false)));
    }

    @Test
    void testCreateForFieldLoadingDataProperty() throws Exception {
        final Descriptor desc = new EntityDescriptor();
        when(metamodelMocks.forOwlClassA().stringAttribute().getFetchType()).thenReturn(FetchType.LAZY);
        final AxiomDescriptor res = sut.createForFieldLoading(PK, OWLClassA.getStrAttField(),
                desc, metamodelMocks.forOwlClassA().entityType());
        assertNotNull(res);
        assertEquals(1, res.getAssertions().size());
        assertTrue(res.getAssertions().contains(
                Assertion.createDataPropertyAssertion(stringAttAUri, Generators.LANG, false)));
    }

    @Test
    void testCreateForFieldLoadingObjectPropertyInEntityContext() throws Exception {
        final Descriptor desc = new EntityDescriptor(false);
        desc.addAttributeDescriptor(metamodelMocks.forOwlClassD().owlClassAAtt(), new EntityDescriptor(CONTEXT));
        final AxiomDescriptor res = sut.createForFieldLoading(PK,
                OWLClassD.getOwlClassAField(), desc, metamodelMocks.forOwlClassD().entityType());
        assertEquals(1, res.getAssertions().size());
        final Assertion as = res.getAssertions().iterator().next();
        assertEquals(Assertion.createObjectPropertyAssertion(owlClassAAttUri, false), as);
        assertEquals(Collections.singleton(CONTEXT), res.getAssertionContexts(as));
    }

    @Test
    void testCreateForFieldLoadingTypes() throws Exception {
        final Descriptor desc = new EntityDescriptor(CONTEXT);
        final AxiomDescriptor res = sut.createForFieldLoading(PK, OWLClassA.getTypesField(),
                desc, metamodelMocks.forOwlClassA().entityType());
        assertEquals(1, res.getAssertions().size());
        final Assertion as = res.getAssertions().iterator().next();
        assertEquals(Assertion.createClassAssertion(metamodelMocks.forOwlClassA().typesSpec().isInferred()), as);
        assertEquals(Collections.singleton(CONTEXT), res.getAssertionContexts(as));
    }

    @Test
    void testCreateForFieldLoadingProperties() throws Exception {
        final Descriptor desc = new EntityDescriptor();
        final AxiomDescriptor res = sut.createForFieldLoading(PK,
                OWLClassB.getPropertiesField(), desc, metamodelMocks.forOwlClassB().entityType());
        assertEquals(1, res.getAssertions().size());
        final Assertion as = res.getAssertions().iterator().next();
        assertEquals(Assertion
                        .createUnspecifiedPropertyAssertion(metamodelMocks.forOwlClassB().propertiesSpec().isInferred()),
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
        return new LoadingParameters<>(cls, PK, descriptor);
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
                .create(Vocabulary.p_a_stringAttribute))).findAny();
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
    void createForFieldLoadingSetsLanguageTagBasedOnDescriptorLanguageTag() throws Exception {
        final Descriptor descriptor = new EntityDescriptor();
        descriptor.setLanguage("en");
        final AxiomDescriptor res = sut.createForFieldLoading(PK, OWLClassA.getStrAttField(), descriptor,
                metamodelMocks.forOwlClassA().entityType());
        final Set<Assertion> assertions = res.getAssertions();
        assertEquals(1, assertions.size());
        assertEquals("en", assertions.iterator().next().getLanguage());
    }

    @Test
    void createForFieldLoadingSetsLanguageTagBasedOnAttributeLanguageTagInDescriptor() throws Exception {
        final Descriptor descriptor = new EntityDescriptor();
        descriptor.setAttributeLanguage(metamodelMocks.forOwlClassA().stringAttribute(), "cs");
        final AxiomDescriptor res = sut.createForFieldLoading(PK, OWLClassA.getStrAttField(), descriptor,
                metamodelMocks.forOwlClassA().entityType());
        final Set<Assertion> assertions = res.getAssertions();
        assertEquals(1, assertions.size());
        assertEquals("cs", assertions.iterator().next().getLanguage());
    }

    @Test
    void createForFieldLoadingSetsLanguageOfPUWhenDescriptorLanguageIsNotSpecified() throws Exception {
        this.sut = new AxiomDescriptorFactory();
        final AxiomDescriptor res = sut.createForFieldLoading(PK, OWLClassA.getStrAttField(), descriptor,
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
    void createForFieldLoadingAllowsOverridingPULevelLanguageSetting() throws Exception {
        final Descriptor descriptor = new EntityDescriptor();
        descriptor.setAttributeLanguage(metamodelMocks.forOwlClassA().stringAttribute(), null);
        final AxiomDescriptor res = sut.createForFieldLoading(PK, OWLClassA.getStrAttField(), descriptor,
                metamodelMocks.forOwlClassA().entityType());
        final Set<Assertion> assertions = res.getAssertions();
        assertEquals(1, assertions.size());
        assertFalse(assertions.iterator().next().hasLanguage());
        assertNull(assertions.iterator().next().getLanguage());
    }

    @Test
    void createForLoadingCreatesDataPropertyAssertionWithoutLanguageWhenNoneIsSetForAttributeAndInDescriptor()
            throws Exception {
        final Descriptor descriptor = new EntityDescriptor();
        when(metamodelMocks.forOwlClassA().stringAttribute().hasLanguage()).thenReturn(false);
        final AxiomDescriptor res = sut.createForFieldLoading(PK, OWLClassA.getStrAttField(), descriptor,
                metamodelMocks.forOwlClassA().entityType());
        final Set<Assertion> assertions = res.getAssertions();
        assertEquals(1, assertions.size());
        assertFalse(assertions.iterator().next().hasLanguage());
    }

    @Test
    void createForLoadingCreatesAnnotationPropertyAssertionWithoutLanguageWhenNoneIsSetForAttributeAndInDescriptor()
            throws Exception {
        final Descriptor descriptor = new EntityDescriptor();
        when(metamodelMocks.forOwlClassN().annotationAttribute().hasLanguage()).thenReturn(false);
        final AxiomDescriptor res = sut
                .createForFieldLoading(PK, OWLClassN.getAnnotationPropertyField(), descriptor,
                        metamodelMocks.forOwlClassN().entityType());
        final Set<Assertion> assertions = res.getAssertions();
        assertEquals(1, assertions.size());
        assertFalse(assertions.iterator().next().hasLanguage());
    }

    @Test
    void createForReferenceLoadingCreatesClassAssertionAxiom() {
        final LoadingParameters<OWLClassA> params = new LoadingParameters<>(OWLClassA.class, PK, descriptor);
        final Axiom<NamedResource> result =
                sut.createForReferenceLoading(params.getIdentifier(), metamodelMocks.forOwlClassA().entityType());
        assertNotNull(result);
        assertEquals(Assertion.createClassAssertion(false), result.getAssertion());
        assertEquals(PK, result.getSubject().getIdentifier());
        assertEquals(Vocabulary.c_OwlClassA, result.getValue().stringValue());
    }

    @Test
    void createForEntityLoadingUsesSubjectContextForAssertionWhenAssertionsInSubjectContextIsConfiguredInDescriptor() {
        descriptor.addAttributeDescriptor(metamodelMocks.forOwlClassD().owlClassAAtt(), descriptorInContext);
        final LoadingParameters<OWLClassD> params = new LoadingParameters<>(OWLClassD.class, PK, descriptor);
        final AxiomDescriptor desc = sut.createForEntityLoading(params, metamodelMocks.forOwlClassD().entityType());
        assertEquals(Collections.emptySet(), desc.getAssertionContexts(
                Assertion.createObjectPropertyAssertion(URI.create(Vocabulary.P_HAS_A), false)));
    }

    @Test
    void createForFieldLoadingUsesSubjectContextForAssertionWhenAssertionsInSubjectContextIsConfiguredInDescriptor()
            throws Exception {
        descriptor.addAttributeDescriptor(metamodelMocks.forOwlClassD().owlClassAAtt(), descriptorInContext);
        final AxiomDescriptor desc = sut.createForFieldLoading(PK, OWLClassD.getOwlClassAField(), descriptor,
                metamodelMocks.forOwlClassD().entityType());
        assertEquals(Collections.emptySet(), desc.getAssertionContexts(
                Assertion.createObjectPropertyAssertion(URI.create(Vocabulary.P_HAS_A), false)));
    }

    @Test
    void createForEntityLoadingAddsAssertionWithoutLanguageTagForSimpleLiteralAttribute() {
        final LoadingParameters<OWLClassM> params = new LoadingParameters<>(OWLClassM.class, PK, descriptor);
        final AxiomDescriptor desc = sut
                .createForEntityLoading(params, metamodelMocks.forOwlClassM().entityType());
        final Optional<Assertion> result = desc.getAssertions().stream().filter(a -> a.getIdentifier().equals(URI
                .create(Vocabulary.p_m_simpleLiteral))).findAny();
        assertTrue(result.isPresent());
        assertFalse(result.get().hasLanguage());
    }

    @Test
    void createForFieldLoadingAddsAssertionWithoutLanguageTagForSimpleLiteralAttribute() throws Exception {
        final AxiomDescriptor desc = sut.createForFieldLoading(PK, OWLClassM.getSimpleLiteralField(), descriptor,
                metamodelMocks.forOwlClassM().entityType());
        assertEquals(1, desc.getAssertions().size());
        final Assertion result = desc.getAssertions().iterator().next();
        assertFalse(result.hasLanguage());
    }

    @Test
    void createForLoadingSetsDefaultContextForAssertionInDefaultWhenSubjectContextIsDifferent() {
        descriptorInContext.addAttributeContext(metamodelMocks.forOwlClassA().stringAttribute(), null);
        final LoadingParameters<OWLClassA> params = new LoadingParameters<>(OWLClassA.class, PK, descriptorInContext);

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
        final LoadingParameters<OWLClassU> params = new LoadingParameters<>(OWLClassU.class, PK, descriptor);
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
}

/**
 * Copyright (C) 2019 Czech Technical University in Prague
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

import cz.cvut.kbss.jopa.environment.*;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.model.JOPAPersistenceProperties;
import cz.cvut.kbss.jopa.model.annotations.FetchType;
import cz.cvut.kbss.jopa.model.annotations.OWLAnnotationProperty;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;
import cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.Attribute.PersistentAttributeType;
import cz.cvut.kbss.jopa.sessions.LoadingParameters;
import cz.cvut.kbss.jopa.utils.Configuration;
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
    private Configuration configuration;

    private AxiomDescriptorFactory factory;

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
        this.configuration = new Configuration(Collections.emptyMap());

        factory = new AxiomDescriptorFactory(configuration);
    }

    @Test
    void testCreateForEntityLoadingWithTypes() {
        final AxiomDescriptor res = factory
                .createForEntityLoading(new LoadingParameters<>(OWLClassA.class, PK, descriptor),
                        metamodelMocks.forOwlClassA().entityType());
        // Types specification and the string attribute
        assertEquals(2, res.getAssertions().size());
        assertEquals(NamedResource.create(PK), res.getSubject());
        assertNull(res.getSubjectContext());
        assertTrue(res.getAssertions().contains(Assertion.createDataPropertyAssertion(stringAttAUri, false)));
        assertTrue(res.getAssertions().contains(Assertion.createClassAssertion(false)));
    }

    @Test
    void testCreateForEntityLoadingWithTypesInContext() throws Exception {
        descriptor.addAttributeContext(OWLClassA.getTypesField(), CONTEXT);
        final AxiomDescriptor res = factory
                .createForEntityLoading(new LoadingParameters<>(OWLClassA.class, PK, descriptor),
                        metamodelMocks.forOwlClassA().entityType());
        // Types specification and the string attribute
        assertEquals(2, res.getAssertions().size());
        assertEquals(NamedResource.create(PK), res.getSubject());
        assertNull(res.getSubjectContext());
        assertTrue(res.getAssertions().contains(
                Assertion.createDataPropertyAssertion(stringAttAUri, false)));
        assertTrue(res.getAssertions().contains(Assertion.createClassAssertion(false)));
        assertEquals(CONTEXT, res.getAssertionContext(Assertion.createClassAssertion(false)));
    }

    @Test
    void testCreateForEntityLoadingWithPropertiesAndContext() {
        final AxiomDescriptor res = factory
                .createForEntityLoading(new LoadingParameters<>(OWLClassB.class, PK, descriptorInContext),
                        metamodelMocks.forOwlClassB().entityType());
        // Class assertion, properties specification and the string attribute
        assertEquals(3, res.getAssertions().size());
        assertEquals(NamedResource.create(PK), res.getSubject());
        assertEquals(CONTEXT, res.getSubjectContext());
        assertTrue(res.getAssertions().contains(Assertion.createDataPropertyAssertion(stringAttBUri, false)));
    }

    @Test
    void testCreateForEntityLoadingWithObjectPropertyInContext() throws Exception {
        descriptor.addAttributeContext(OWLClassD.getOwlClassAField(), CONTEXT);
        final AxiomDescriptor res = factory
                .createForEntityLoading(new LoadingParameters<>(OWLClassD.class, PK, descriptor),
                        metamodelMocks.forOwlClassD().entityType());
        // Class assertion and the object property assertion
        assertEquals(2, res.getAssertions().size());
        assertEquals(NamedResource.create(PK), res.getSubject());
        assertNull(res.getSubjectContext());
        final Optional<Assertion> ass = res.getAssertions().stream()
                                           .filter(a -> a.getIdentifier().equals(owlClassAAttUri)).findAny();
        assertTrue(ass.isPresent());
        assertEquals(CONTEXT, res.getAssertionContext(ass.get()));
        assertEquals(owlClassAAttUri, ass.get().getIdentifier());
    }

    @Test
    void testCreateForEntityLoadingWithAnnotationProperty() {
        // Artificially change the attribute type to annotation
        when(metamodelMocks.forOwlClassD().owlClassAAtt().getPersistentAttributeType()).thenReturn(
                PersistentAttributeType.ANNOTATION);
        final AxiomDescriptor res = factory
                .createForEntityLoading(new LoadingParameters<>(OWLClassD.class, PK, descriptor),
                        metamodelMocks.forOwlClassD().entityType());
        // Class assertion and the annotation property assertion
        assertEquals(2, res.getAssertions().size());
        assertEquals(NamedResource.create(PK), res.getSubject());
        assertNull(res.getSubjectContext());
        assertTrue(res.getAssertions().contains(
                Assertion.createAnnotationPropertyAssertion(owlClassAAttUri, false)));
    }

    @Test
    void createForEntityLoadingWithLazilyLoadedAttribute() {
        when(metamodelMocks.forOwlClassA().stringAttribute().getFetchType()).thenReturn(FetchType.LAZY);
        final AxiomDescriptor res = factory
                .createForEntityLoading(new LoadingParameters<>(OWLClassA.class, PK, descriptor),
                        metamodelMocks.forOwlClassA().entityType());
        // Types specification (class assertion)
        assertEquals(1, res.getAssertions().size());
        assertEquals(NamedResource.create(PK), res.getSubject());
        assertNull(res.getSubjectContext());
        assertFalse(res.getAssertions().contains(
                Assertion.createDataPropertyAssertion(stringAttAUri, false)));
        assertTrue(res.getAssertions().contains(Assertion.createClassAssertion(false)));
    }

    @Test
    void testCreateForFieldLoadingDataProperty() throws Exception {
        final Descriptor desc = new EntityDescriptor();
        when(metamodelMocks.forOwlClassA().stringAttribute().getFetchType()).thenReturn(FetchType.LAZY);
        final AxiomDescriptor res = factory.createForFieldLoading(PK, OWLClassA.getStrAttField(),
                desc, metamodelMocks.forOwlClassA().entityType());
        assertNotNull(res);
        assertEquals(1, res.getAssertions().size());
        assertTrue(res.getAssertions().contains(
                Assertion.createDataPropertyAssertion(stringAttAUri, false)));
    }

    @Test
    void testCreateForFieldLoadingObjectPropertyInEntityContext() throws Exception {
        final Descriptor desc = new EntityDescriptor();
        desc.addAttributeDescriptor(OWLClassD.getOwlClassAField(), new EntityDescriptor(CONTEXT));
        final AxiomDescriptor res = factory.createForFieldLoading(PK,
                OWLClassD.getOwlClassAField(), desc, metamodelMocks.forOwlClassD().entityType());
        assertEquals(1, res.getAssertions().size());
        final Assertion as = res.getAssertions().iterator().next();
        assertEquals(Assertion.createObjectPropertyAssertion(owlClassAAttUri, false), as);
        assertEquals(CONTEXT, res.getAssertionContext(as));
    }

    @Test
    void testCreateForFieldLoadingTypes() throws Exception {
        final Descriptor desc = new EntityDescriptor(CONTEXT);
        final AxiomDescriptor res = factory.createForFieldLoading(PK, OWLClassA.getTypesField(),
                desc, metamodelMocks.forOwlClassA().entityType());
        assertEquals(1, res.getAssertions().size());
        final Assertion as = res.getAssertions().iterator().next();
        assertEquals(Assertion.createClassAssertion(metamodelMocks.forOwlClassA().typesSpec().isInferred()), as);
        assertEquals(CONTEXT, res.getAssertionContext(as));
    }

    @Test
    void testCreateForFieldLoadingProperties() throws Exception {
        final Descriptor desc = new EntityDescriptor();
        final AxiomDescriptor res = factory.createForFieldLoading(PK,
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
        final AxiomDescriptor res = factory.createForEntityLoading(loadingParameters(OWLClassQ.class, desc),
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
        final AxiomDescriptor res = factory.createForEntityLoading(loadingParameters(OWLClassA.class, descriptor),
                metamodelMocks.forOwlClassA().entityType());
        final Set<Assertion> assertions = res.getAssertions();
        assertions.stream().filter(a -> a.getType() != Assertion.AssertionType.CLASS && a.getType() !=
                Assertion.AssertionType.OBJECT_PROPERTY).forEach(a -> assertEquals("en", a.getLanguage()));
    }

    @Test
    void createForEntityLoadingSetsLanguageTagOfSpecificAssertionAccordingToDescriptor() throws Exception {
        final Descriptor descriptor = new EntityDescriptor();
        descriptor.setLanguage("en");
        descriptor.setAttributeLanguage(OWLClassA.getStrAttField(), "cs");
        final AxiomDescriptor res = factory.createForEntityLoading(loadingParameters(OWLClassA.class, descriptor),
                metamodelMocks.forOwlClassA().entityType());
        final Set<Assertion> assertions = res.getAssertions();
        final Optional<Assertion> strAssertion = assertions.stream().filter(a -> a.getIdentifier().equals(URI
                .create(Vocabulary.p_a_stringAttribute))).findAny();
        assertTrue(strAssertion.isPresent());
        assertEquals("cs", strAssertion.get().getLanguage());
    }

    @Test
    void createForEntityLoadingSetsLanguageTagAccordingToGlobalPUSpecification() {
        configuration.set(JOPAPersistenceProperties.LANG, "en");
        this.factory = new AxiomDescriptorFactory(configuration);
        final AxiomDescriptor res = factory.createForEntityLoading(loadingParameters(OWLClassA.class, descriptor),
                metamodelMocks.forOwlClassA().entityType());
        final Set<Assertion> assertions = res.getAssertions();
        assertions.stream().filter(a -> a.getType() != Assertion.AssertionType.CLASS && a.getType() !=
                Assertion.AssertionType.OBJECT_PROPERTY).forEach(a -> assertEquals("en", a.getLanguage()));
    }

    @Test
    void createForFieldLoadingSetsLanguageTagBasedOnDescriptorLanguageTag() throws Exception {
        final Descriptor descriptor = new EntityDescriptor();
        descriptor.setLanguage("en");
        final AxiomDescriptor res = factory.createForFieldLoading(PK, OWLClassA.getStrAttField(), descriptor,
                metamodelMocks.forOwlClassA().entityType());
        final Set<Assertion> assertions = res.getAssertions();
        assertEquals(1, assertions.size());
        assertEquals("en", assertions.iterator().next().getLanguage());
    }

    @Test
    void createForFieldLoadingSetsLanguageTagBasedOnAttributeLanguageTagInDescriptor() throws Exception {
        final Descriptor descriptor = new EntityDescriptor();
        descriptor.setAttributeLanguage(OWLClassA.getStrAttField(), "cs");
        final AxiomDescriptor res = factory.createForFieldLoading(PK, OWLClassA.getStrAttField(), descriptor,
                metamodelMocks.forOwlClassA().entityType());
        final Set<Assertion> assertions = res.getAssertions();
        assertEquals(1, assertions.size());
        assertEquals("cs", assertions.iterator().next().getLanguage());
    }

    @Test
    void createForFieldLoadingSetsLanguageOfPUWhenDescriptorLanguageIsNotSpecified() throws Exception {
        configuration.set(JOPAPersistenceProperties.LANG, "cs");
        this.factory = new AxiomDescriptorFactory(configuration);
        final AxiomDescriptor res = factory.createForFieldLoading(PK, OWLClassA.getStrAttField(), descriptor,
                metamodelMocks.forOwlClassA().entityType());
        final Set<Assertion> assertions = res.getAssertions();
        assertEquals(1, assertions.size());
        assertEquals("cs", assertions.iterator().next().getLanguage());
    }

    @Test
    void createForEntityLoadingAllowsOverridingPULevelLanguageSetting() {
        configuration.set(JOPAPersistenceProperties.LANG, "en");
        descriptor.setLanguage(null);
        final AxiomDescriptor res = factory.createForEntityLoading(loadingParameters(OWLClassA.class, descriptor),
                metamodelMocks.forOwlClassA().entityType());
        final Set<Assertion> assertions = res.getAssertions();
        assertions.stream().filter(a -> a.getType() != Assertion.AssertionType.CLASS && a.getType() !=
                Assertion.AssertionType.OBJECT_PROPERTY).forEach(a -> {
            assertTrue(a.hasLanguage());
            assertNull(a.getLanguage());
        });
    }

    @Test
    void createForFieldLoadingAllowsOverridingPULevelLanguageSetting() throws Exception {
        configuration.set(JOPAPersistenceProperties.LANG, "en");
        final Descriptor descriptor = new EntityDescriptor();
        descriptor.setAttributeLanguage(OWLClassA.getStrAttField(), null);
        final AxiomDescriptor res = factory.createForFieldLoading(PK, OWLClassA.getStrAttField(), descriptor,
                metamodelMocks.forOwlClassA().entityType());
        final Set<Assertion> assertions = res.getAssertions();
        assertEquals(1, assertions.size());
        assertTrue(assertions.iterator().next().hasLanguage());
        assertNull(assertions.iterator().next().getLanguage());
    }

    @Test
    void createForLoadingCreatesDataPropertyAssertionWithoutLanguageWhenNoneIsSetForPUAndInDescriptor()
            throws Exception {
        final Descriptor descriptor = new EntityDescriptor();
        final AxiomDescriptor res = factory.createForFieldLoading(PK, OWLClassA.getStrAttField(), descriptor,
                metamodelMocks.forOwlClassA().entityType());
        final Set<Assertion> assertions = res.getAssertions();
        assertEquals(1, assertions.size());
        assertFalse(assertions.iterator().next().hasLanguage());
    }

    @Test
    void createForLoadingCreatesAnnotationPropertyAssertionWithoutLanguageWhenNoneIsSetForPUAndInDescriptor()
            throws Exception {
        final Descriptor descriptor = new EntityDescriptor();
        final AxiomDescriptor res = factory
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
                factory.createForReferenceLoading(params.getIdentifier(), metamodelMocks.forOwlClassA().entityType());
        assertNotNull(result);
        assertEquals(Assertion.createClassAssertion(false), result.getAssertion());
        assertEquals(PK, result.getSubject().getIdentifier());
        assertEquals(Vocabulary.c_OwlClassA, result.getValue().stringValue());
    }
}

/*
 * JOPA
 * Copyright (C) 2024 Czech Technical University in Prague
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

import cz.cvut.kbss.jopa.environment.*;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.exceptions.CardinalityConstraintViolatedException;
import cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.AbstractAttribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.vocabulary.OWL;
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
import java.util.Collections;
import java.util.List;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class SingularObjectPropertyStrategyTest {

    private static final URI IDENTIFIER = Generators.createIndividualIdentifier();
    private static final URI VALUE = Generators.createIndividualIdentifier();

    @Mock
    private EntityMappingHelper mapperMock;

    @Mock
    private ReferenceSavingResolver referenceResolverMock;


    private MetamodelMocks metamodelMocks;

    private Descriptor descriptor;

    private AxiomValueGatherer gatherer;

    @BeforeEach
    void setUp() throws Exception {
        this.metamodelMocks = new MetamodelMocks();
        this.gatherer = spy(new AxiomValueGatherer(NamedResource.create(IDENTIFIER), null));
        this.descriptor = spy(new EntityDescriptor());
    }

    @Test
    void buildInstanceFieldSupportsPlainIdentifierValues() throws Exception {
        final FieldStrategy<? extends FieldSpecification<? super OWLClassP, ?>, OWLClassP> strategy =
                strategy(metamodelMocks.forOwlClassP().entityType(), metamodelMocks.forOwlClassP().pUriAttribute());
        strategy.setReferenceSavingResolver(referenceResolverMock);
        strategy.addAxiomValue(
                new AxiomImpl<>(NamedResource.create(IDENTIFIER), propertyP(),
                        new Value<>(NamedResource.create(VALUE))));
        final OWLClassP p = new OWLClassP();
        strategy.buildInstanceFieldValue(p);
        assertEquals(VALUE, p.getIndividualUri());
    }

    private <T> FieldStrategy<? extends FieldSpecification<? super T, ?>, T> strategy(EntityType<T> et,
                                                                                      AbstractAttribute<? super T, ?> att) {
        return new SingularObjectPropertyStrategy<>(et, att, descriptor, mapperMock);
    }

    private Assertion propertyP() throws Exception {
        final URI uri = URI.create(OWLClassP.getIndividualUriField().getAnnotation(OWLObjectProperty.class).iri());
        return Assertion.createObjectPropertyAssertion(uri, false);
    }

    @Test
    void buildsAxiomFromPlainIdentifierValue() throws Exception {
        final OWLClassP p = new OWLClassP();
        p.setUri(IDENTIFIER);
        p.setIndividualUri(VALUE);
        final FieldStrategy<? extends FieldSpecification<? super OWLClassP, ?>, OWLClassP> strategy =
                strategy(metamodelMocks.forOwlClassP().entityType(), metamodelMocks.forOwlClassP().pUriAttribute());
        strategy.setReferenceSavingResolver(referenceResolverMock);
        when(referenceResolverMock.shouldSaveReference(p.getIndividualUri(), Collections.emptySet())).thenReturn(true);
        strategy.buildAxiomValuesFromInstance(p, gatherer);
        final AxiomValueDescriptor valueDescriptor = OOMTestUtils.getAxiomValueDescriptor(gatherer);
        assertEquals(1, valueDescriptor.getAssertions().size());
        final List<Value<?>> axioms = valueDescriptor
                .getAssertionValues(propertyP());
        assertEquals(1, axioms.size());
        assertEquals(NamedResource.create(VALUE), axioms.get(0).getValue());
    }

    @Test
    void buildAxiomValuesChecksWhetherReferenceCanBeSaved() {
        final OWLClassD d = new OWLClassD();
        d.setUri(IDENTIFIER);
        d.setOwlClassA(Generators.generateOwlClassAInstance());
        final FieldStrategy<? extends FieldSpecification<? super OWLClassD, ?>, OWLClassD> strategy =
                strategy(metamodelMocks.forOwlClassD().entityType(), metamodelMocks.forOwlClassD().owlClassAAtt());
        when(mapperMock.getEntityType(OWLClassA.class)).thenReturn(metamodelMocks.forOwlClassA().entityType());
        strategy.setReferenceSavingResolver(referenceResolverMock);
        strategy.buildAxiomValuesFromInstance(d, gatherer);

        verify(referenceResolverMock).shouldSaveReference(d.getOwlClassA(), descriptor.getContexts());
    }

    @Test
    void buildAxiomValuesRegistersPendingChangeWhenReferenceCannotBeSavedDirectly() {
        final OWLClassD d = new OWLClassD();
        d.setUri(IDENTIFIER);
        d.setOwlClassA(Generators.generateOwlClassAInstance());
        final AbstractAttribute<OWLClassD, OWLClassA> att = metamodelMocks.forOwlClassD().owlClassAAtt();
        final FieldStrategy<? extends FieldSpecification<? super OWLClassD, ?>, OWLClassD> strategy =
                strategy(metamodelMocks.forOwlClassD().entityType(), att);
        when(mapperMock.getEntityType(OWLClassA.class)).thenReturn(metamodelMocks.forOwlClassA().entityType());
        when(referenceResolverMock.shouldSaveReference(d.getOwlClassA(), null)).thenReturn(false);
        strategy.setReferenceSavingResolver(referenceResolverMock);
        strategy.buildAxiomValuesFromInstance(d, gatherer);

        verify(referenceResolverMock)
                .registerPendingReference(NamedResource.create(IDENTIFIER), strategy.createAssertion(),
                        d.getOwlClassA(), null);
    }

    @Test
    void buildAxiomValuesAddsNullValueToAxiomBuilderForNullAttributeValue() {
        final OWLClassD d = new OWLClassD();
        d.setUri(IDENTIFIER);
        final AbstractAttribute<OWLClassD, OWLClassA> att = metamodelMocks.forOwlClassD().owlClassAAtt();
        final FieldStrategy<? extends FieldSpecification<? super OWLClassD, ?>, OWLClassD> strategy =
                strategy(metamodelMocks.forOwlClassD().entityType(), att);
        when(mapperMock.getEntityType(OWLClassA.class)).thenReturn(metamodelMocks.forOwlClassA().entityType());
        when(referenceResolverMock.shouldSaveReference(d.getOwlClassA(), null)).thenReturn(true);
        strategy.setReferenceSavingResolver(referenceResolverMock);
        strategy.buildAxiomValuesFromInstance(d, gatherer);

        verify(gatherer).addValue(strategy.createAssertion(), Value.nullValue(), null);
    }

    @Test
    void buildAxiomValueAddsNullValueToAxiomBuilderWhenReferenceIsRegisteredAsPending() {
        final OWLClassD d = new OWLClassD();
        d.setUri(IDENTIFIER);
        final OWLClassA a = new OWLClassA();
        d.setOwlClassA(a);
        a.setUri(Generators.createIndividualIdentifier());
        final AbstractAttribute<OWLClassD, OWLClassA> att = metamodelMocks.forOwlClassD().owlClassAAtt();
        final FieldStrategy<? extends FieldSpecification<? super OWLClassD, ?>, OWLClassD> strategy =
                strategy(metamodelMocks.forOwlClassD().entityType(), att);
        when(mapperMock.getEntityType(OWLClassA.class)).thenReturn(metamodelMocks.forOwlClassA().entityType());
        when(referenceResolverMock.shouldSaveReference(a, null)).thenReturn(false);
        strategy.setReferenceSavingResolver(referenceResolverMock);
        strategy.buildAxiomValuesFromInstance(d, gatherer);

        verify(gatherer).addValue(strategy.createAssertion(), Value.nullValue(), null);
    }

    @Test
    void buildAxiomValueAddsReferenceForPolymorphicAttribute() {
        final OWLClassT instance = new OWLClassT();
        instance.setUri(IDENTIFIER);
        final OWLClassR r = new OWLClassR(Generators.createIndividualIdentifier());
        instance.setOwlClassS(r);
        when(mapperMock.getEntityType(OWLClassR.class)).thenReturn(metamodelMocks.forOwlClassR().entityType());
        when(mapperMock.getEntityType(OWLClassS.class)).thenReturn(metamodelMocks.forOwlClassS().entityType());
        when(referenceResolverMock.shouldSaveReference(any(), any())).thenReturn(true);
        final FieldStrategy<? extends FieldSpecification<? super OWLClassT, ?>, OWLClassT> sut = strategy(
                metamodelMocks.forOwlClassT().entityType(), metamodelMocks.forOwlClassT().tOwlClassSAtt());
        sut.setReferenceSavingResolver(referenceResolverMock);
        sut.buildAxiomValuesFromInstance(instance, gatherer);

        verify(referenceResolverMock).shouldSaveReference(r, Collections.emptySet());
        verify(referenceResolverMock, never()).registerPendingReference(any(), any(), any(), any());
        verify(gatherer).addValue(sut.createAssertion(), new Value<>(NamedResource.create(r.getUri())), null);
    }

    @Test
    void addValueThrowsCardinalityViolationWhenValueIsAlreadyPresent() {
        final FieldStrategy<? extends FieldSpecification<? super OWLClassD, ?>, OWLClassD> sut =
                strategy(metamodelMocks.forOwlClassD().entityType(), metamodelMocks.forOwlClassD().owlClassAAtt());
        final OWLClassA existing = Generators.generateOwlClassAInstance();
        when(mapperMock.getEntityFromCacheOrOntology(eq(OWLClassA.class), eq(VALUE), any())).thenReturn(existing);
        final Assertion assertion = Assertion.createObjectPropertyAssertion(URI.create(Vocabulary.P_HAS_A), false);
        sut.addAxiomValue(
                new AxiomImpl<>(NamedResource.create(IDENTIFIER), assertion, new Value<>(NamedResource.create(VALUE))));
        final OWLClassA another = Generators.generateOwlClassAInstance();
        when(mapperMock.getEntityFromCacheOrOntology(eq(OWLClassA.class), eq(another.getUri()), any()))
                .thenReturn(another);
        final Axiom<NamedResource> violationAxiom = new AxiomImpl<>(NamedResource.create(IDENTIFIER), assertion,
                new Value<>(
                        NamedResource.create(another.getUri())));
        assertThrows(CardinalityConstraintViolatedException.class, () -> sut.addAxiomValue(violationAxiom));
    }

    @Test
    void addValueDoesNothingWhenReferenceInstanceCannotBeLoaded() {
        final FieldStrategy<? extends FieldSpecification<? super OWLClassD, ?>, OWLClassD> sut =
                strategy(metamodelMocks.forOwlClassD().entityType(), metamodelMocks.forOwlClassD().owlClassAAtt());
        final OWLClassA existing = Generators.generateOwlClassAInstance();
        when(mapperMock.getEntityFromCacheOrOntology(eq(OWLClassA.class), eq(VALUE), any())).thenReturn(existing);
        final Assertion assertion = Assertion.createObjectPropertyAssertion(URI.create(Vocabulary.P_HAS_A), false);
        sut.addAxiomValue(
                new AxiomImpl<>(NamedResource.create(IDENTIFIER), assertion, new Value<>(NamedResource.create(VALUE))));
        final URI another = Generators.createIndividualIdentifier();
        when(mapperMock.getEntityFromCacheOrOntology(eq(OWLClassA.class), eq(another), any())).thenReturn(null);
        sut.addAxiomValue(new AxiomImpl<>(NamedResource.create(IDENTIFIER), assertion,
                new Value<>(NamedResource.create(another))));
        final OWLClassD instance = new OWLClassD();
        sut.buildInstanceFieldValue(instance);
        assertSame(existing, instance.getOwlClassA());
        verify(mapperMock).getEntityFromCacheOrOntology(eq(OWLClassA.class), eq(another), any());
    }

    @Test
    void addValueGetsAttributeDescriptorFromEntityDescriptorForLoading() {
        final EntityDescriptor aDescriptor = new EntityDescriptor(Generators.createIndividualIdentifier());
        descriptor.addAttributeDescriptor(metamodelMocks.forOwlClassD().owlClassAAtt(), aDescriptor);
        final FieldStrategy<? extends FieldSpecification<? super OWLClassD, ?>, OWLClassD> sut =
                strategy(metamodelMocks.forOwlClassD().entityType(), metamodelMocks.forOwlClassD().owlClassAAtt());
        final OWLClassA existing = Generators.generateOwlClassAInstance();
        when(mapperMock.getEntityFromCacheOrOntology(eq(OWLClassA.class), eq(VALUE), any())).thenReturn(existing);
        final Assertion assertion = Assertion.createObjectPropertyAssertion(URI.create(Vocabulary.P_HAS_A), false);
        sut.addAxiomValue(
                new AxiomImpl<>(NamedResource.create(IDENTIFIER), assertion, new Value<>(NamedResource.create(VALUE))));
        verify(mapperMock).getEntityFromCacheOrOntology(OWLClassA.class, VALUE, aDescriptor);
    }

    @Test
    void buildAxiomValuesFromInstanceChecksForReferenceExistenceUsingTargetReferenceContext() {
        final EntityDescriptor aDescriptor = new EntityDescriptor(Generators.createIndividualIdentifier());
        descriptor.addAttributeDescriptor(metamodelMocks.forOwlClassD().owlClassAAtt(), aDescriptor);
        final FieldStrategy<? extends FieldSpecification<? super OWLClassD, ?>, OWLClassD> sut =
                strategy(metamodelMocks.forOwlClassD().entityType(), metamodelMocks.forOwlClassD().owlClassAAtt());
        sut.setReferenceSavingResolver(referenceResolverMock);
        final OWLClassD instance = new OWLClassD(Generators.createIndividualIdentifier());
        instance.setOwlClassA(Generators.generateOwlClassAInstance());
        sut.buildAxiomValuesFromInstance(instance, gatherer);
        verify(referenceResolverMock).shouldSaveReference(instance.getOwlClassA(), aDescriptor.getContexts());
    }

    @Test
    void buildAxiomsFromInstanceReturnsAxiomsCorrespondingToAttributeValue() {
        final OWLClassD d = new OWLClassD();
        d.setUri(IDENTIFIER);
        d.setOwlClassA(Generators.generateOwlClassAInstance());
        final FieldStrategy<? extends FieldSpecification<? super OWLClassD, ?>, OWLClassD> strategy =
                strategy(metamodelMocks.forOwlClassD().entityType(), metamodelMocks.forOwlClassD().owlClassAAtt());
        when(mapperMock.getEntityType(OWLClassA.class)).thenReturn(metamodelMocks.forOwlClassA().entityType());
        strategy.setReferenceSavingResolver(referenceResolverMock);
        strategy.buildAxiomValuesFromInstance(d, gatherer);

        final Set<Axiom<?>> result = strategy.buildAxiomsFromInstance(d);
        assertEquals(1, result.size());
        final Axiom<?> ax = result.iterator().next();
        assertEquals(NamedResource.create(IDENTIFIER), ax.getSubject());
        assertEquals(metamodelMocks.forOwlClassD().owlClassAAtt().getIRI().toURI(), ax.getAssertion().getIdentifier());
        assertEquals(NamedResource.create(d.getOwlClassA().getUri()), ax.getValue().getValue());
    }

    @Test
    void buildAxiomsFromInstanceReturnsEmptyCollectionWhenAttributeValueIsNull() {
        final OWLClassD d = new OWLClassD();
        d.setUri(IDENTIFIER);
        d.setOwlClassA(null);
        final FieldStrategy<? extends FieldSpecification<? super OWLClassD, ?>, OWLClassD> strategy =
                strategy(metamodelMocks.forOwlClassD().entityType(), metamodelMocks.forOwlClassD().owlClassAAtt());
        when(mapperMock.getEntityType(OWLClassA.class)).thenReturn(metamodelMocks.forOwlClassA().entityType());

        final Set<Axiom<?>> result = strategy.buildAxiomsFromInstance(d);
        assertNotNull(result);
        assertTrue(result.isEmpty());
    }

    @Test
    void addValueFromAxiomConvertsNamedResourceToEnumConstantForEnumValuedObjectProperty() {
        final OWLClassM m = new OWLClassM();
        m.setKey(IDENTIFIER.toString());
        final NamedResource value = NamedResource.create(OWL.ANNOTATION_PROPERTY);
        final FieldStrategy<? extends FieldSpecification<? super OWLClassM, ?>, OWLClassM> sut =
                strategy(metamodelMocks.forOwlClassM().entityType(),
                        metamodelMocks.forOwlClassM().objectOneOfEnumAttribute());
        sut.addAxiomValue(new AxiomImpl<>(NamedResource.create(IDENTIFIER), Assertion.createObjectPropertyAssertion(
                URI.create(Vocabulary.p_m_objectOneOfEnumAttribute), false), new Value<>(value)));
        sut.buildInstanceFieldValue(m);
        assertEquals(OneOfEnum.ANNOTATION_PROPERTY, m.getObjectOneOfEnumAttribute());
    }

    @Test
    void buildAxiomValuesFromInstanceConvertsEnumValueToNamedResourceForEnumValuedObjectProperty() {
        final OWLClassM m = new OWLClassM();
        m.setKey(IDENTIFIER.toString());
        m.setObjectOneOfEnumAttribute(OneOfEnum.DATATYPE_PROPERTY);
        final FieldStrategy<? extends FieldSpecification<? super OWLClassM, ?>, OWLClassM> sut =
                strategy(metamodelMocks.forOwlClassM().entityType(),
                        metamodelMocks.forOwlClassM().objectOneOfEnumAttribute());
        sut.setReferenceSavingResolver(referenceResolverMock);
        when(referenceResolverMock.shouldSaveReference(m.getObjectOneOfEnumAttribute(), Collections.emptySet())).thenReturn(true);
        sut.buildAxiomValuesFromInstance(m, gatherer);
        verify(gatherer).addValue(Assertion.createObjectPropertyAssertion(
                        URI.create(Vocabulary.p_m_objectOneOfEnumAttribute), false),
                new Value<>(NamedResource.create(OWL.DATATYPE_PROPERTY)), null);
    }

    @Test
    void lazilyAddAxiomValueSetsLazyLoadingPlaceholderAndDoesNotLoadReference() {
        final FieldStrategy<? extends FieldSpecification<? super OWLClassD, ?>, OWLClassD> sut =
                strategy(metamodelMocks.forOwlClassD().entityType(), metamodelMocks.forOwlClassD().owlClassAAtt());
        sut.setReferenceSavingResolver(referenceResolverMock);
        sut.lazilyAddAxiomValue(
                new AxiomImpl<>(NamedResource.create(IDENTIFIER), Assertion.createObjectPropertyAssertion(URI.create(Vocabulary.P_HAS_A), false),
                        new Value<>(NamedResource.create(VALUE))));
        verify(mapperMock, never()).getEntityFromCacheOrOntology(eq(OWLClassA.class), eq(VALUE), any());
        assertTrue(sut.hasValue());
    }

    @Test
    void buildInstanceFieldAfterLazilyAddingReferenceDoesNotSetFieldValue() {
        final FieldStrategy<? extends FieldSpecification<? super OWLClassD, ?>, OWLClassD> sut =
                strategy(metamodelMocks.forOwlClassD().entityType(), metamodelMocks.forOwlClassD().owlClassAAtt());
        sut.setReferenceSavingResolver(referenceResolverMock);
        sut.lazilyAddAxiomValue(
                new AxiomImpl<>(NamedResource.create(IDENTIFIER), Assertion.createObjectPropertyAssertion(URI.create(Vocabulary.P_HAS_A), false),
                        new Value<>(NamedResource.create(VALUE))));
        final OWLClassD target = new OWLClassD(IDENTIFIER);
        sut.buildInstanceFieldValue(target);
        assertNull(target.getOwlClassA());
    }
}

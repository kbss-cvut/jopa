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

import cz.cvut.kbss.jopa.environment.*;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.exceptions.CardinalityConstraintViolatedException;
import cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.ontodriver.descriptor.AxiomValueDescriptor;
import cz.cvut.kbss.ontodriver.model.*;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.net.URI;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

class SingularObjectPropertyStrategyTest {

    private static final URI IDENTIFIER = Generators.createIndividualIdentifier();
    private static final URI VALUE = URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/individualAAA");

    @Mock
    private EntityMappingHelper mapperMock;

    @Mock
    private ReferenceSavingResolver referenceResolverMock;


    private MetamodelMocks metamodelMocks;

    private Descriptor descriptor = new EntityDescriptor();

    private AxiomValueGatherer gatherer;

    @BeforeEach
    void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        this.metamodelMocks = new MetamodelMocks();
        this.gatherer = spy(new AxiomValueGatherer(NamedResource.create(IDENTIFIER), null));
    }

    @Test
    void buildInstanceFieldSupportsPlainIdentifierValues() throws Exception {
        final FieldStrategy<? extends FieldSpecification<? super OWLClassP, ?>, OWLClassP> strategy =
                strategy(metamodelMocks.forOwlClassP().entityType(), metamodelMocks.forOwlClassP().pUriAttribute());
        strategy.setReferenceSavingResolver(referenceResolverMock);
        strategy.addValueFromAxiom(
                new AxiomImpl<>(NamedResource.create(IDENTIFIER), propertyP(),
                        new Value<>(NamedResource.create(VALUE))));
        final OWLClassP p = new OWLClassP();
        strategy.buildInstanceFieldValue(p);
        assertEquals(VALUE, p.getIndividualUri());
    }

    private <T> FieldStrategy<? extends FieldSpecification<? super T, ?>, T> strategy(EntityType<T> et,
                                                                                      Attribute<? super T, ?> att) {
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
        when(referenceResolverMock.shouldSaveReference(p.getIndividualUri(), null)).thenReturn(true);
        strategy.buildAxiomValuesFromInstance(p, gatherer);
        final AxiomValueDescriptor valueDescriptor = OOMTestUtils.getAxiomValueDescriptor(gatherer);
        assertEquals(1, valueDescriptor.getAssertions().size());
        final List<Value<?>> axioms = valueDescriptor
                .getAssertionValues(propertyP());
        assertEquals(1, axioms.size());
        assertEquals(NamedResource.create(VALUE), axioms.get(0).getValue());
    }

    @Test
    void buildAxiomValuesChecksWhetherReferenceCanBeSaved() throws Exception {
        final OWLClassD d = new OWLClassD();
        d.setUri(IDENTIFIER);
        d.setOwlClassA(Generators.generateOwlClassAInstance());
        final FieldStrategy<? extends FieldSpecification<? super OWLClassD, ?>, OWLClassD> strategy =
                strategy(metamodelMocks.forOwlClassD().entityType(), metamodelMocks.forOwlClassD().owlClassAAtt());
        when(mapperMock.getEntityType(OWLClassA.class)).thenReturn(metamodelMocks.forOwlClassA().entityType());
        strategy.setReferenceSavingResolver(referenceResolverMock);
        strategy.buildAxiomValuesFromInstance(d, gatherer);

        verify(referenceResolverMock).shouldSaveReference(d.getOwlClassA(), descriptor.getContext());
    }

    @Test
    void buildAxiomValuesRegistersPendingChangeWhenReferenceCannotBeSavedDirectly() throws Exception {
        final OWLClassD d = new OWLClassD();
        d.setUri(IDENTIFIER);
        d.setOwlClassA(Generators.generateOwlClassAInstance());
        final Attribute<OWLClassD, OWLClassA> att = metamodelMocks.forOwlClassD().owlClassAAtt();
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
    void buildAxiomValuesAddsNullValueToAxiomBuilderForNullAttributeValue() throws Exception {
        final OWLClassD d = new OWLClassD();
        d.setUri(IDENTIFIER);
        final Attribute<OWLClassD, OWLClassA> att = metamodelMocks.forOwlClassD().owlClassAAtt();
        final FieldStrategy<? extends FieldSpecification<? super OWLClassD, ?>, OWLClassD> strategy =
                strategy(metamodelMocks.forOwlClassD().entityType(), att);
        when(mapperMock.getEntityType(OWLClassA.class)).thenReturn(metamodelMocks.forOwlClassA().entityType());
        when(referenceResolverMock.shouldSaveReference(d.getOwlClassA(), null)).thenReturn(true);
        strategy.setReferenceSavingResolver(referenceResolverMock);
        strategy.buildAxiomValuesFromInstance(d, gatherer);

        verify(gatherer).addValue(strategy.createAssertion(), Value.nullValue(), null);
    }

    @Test
    void buildAxiomValueAddsNullValueToAxiomBuilderWhenReferenceIsRegisteredAsPending() throws Exception {
        final OWLClassD d = new OWLClassD();
        d.setUri(IDENTIFIER);
        final OWLClassA a = new OWLClassA();
        d.setOwlClassA(a);
        a.setUri(Generators.createIndividualIdentifier());
        final Attribute<OWLClassD, OWLClassA> att = metamodelMocks.forOwlClassD().owlClassAAtt();
        final FieldStrategy<? extends FieldSpecification<? super OWLClassD, ?>, OWLClassD> strategy =
                strategy(metamodelMocks.forOwlClassD().entityType(), att);
        when(mapperMock.getEntityType(OWLClassA.class)).thenReturn(metamodelMocks.forOwlClassA().entityType());
        when(referenceResolverMock.shouldSaveReference(a, null)).thenReturn(false);
        strategy.setReferenceSavingResolver(referenceResolverMock);
        strategy.buildAxiomValuesFromInstance(d, gatherer);

        verify(gatherer).addValue(strategy.createAssertion(), Value.nullValue(), null);
    }

    @Test
    void buildAxiomValueAddsReferenceForPolymorphicAttribute() throws Exception {
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

        verify(referenceResolverMock).shouldSaveReference(r, null);
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
        sut.addValueFromAxiom(
                new AxiomImpl<>(NamedResource.create(IDENTIFIER), assertion, new Value<>(NamedResource.create(VALUE))));
        final OWLClassA another = Generators.generateOwlClassAInstance();
        when(mapperMock.getEntityFromCacheOrOntology(eq(OWLClassA.class), eq(another.getUri()), any()))
                .thenReturn(another);
        final Axiom<NamedResource> violationAxiom = new AxiomImpl<>(NamedResource.create(IDENTIFIER), assertion,
                new Value<>(NamedResource.create(another.getUri())));
        assertThrows(CardinalityConstraintViolatedException.class, () -> sut.addValueFromAxiom(violationAxiom));
    }

    @Test
    void addValueDoesNothingWhenReferenceInstanceCannotBeLoaded() throws Exception {
        final FieldStrategy<? extends FieldSpecification<? super OWLClassD, ?>, OWLClassD> sut =
                strategy(metamodelMocks.forOwlClassD().entityType(), metamodelMocks.forOwlClassD().owlClassAAtt());
        final OWLClassA existing = Generators.generateOwlClassAInstance();
        when(mapperMock.getEntityFromCacheOrOntology(eq(OWLClassA.class), eq(VALUE), any())).thenReturn(existing);
        final Assertion assertion = Assertion.createObjectPropertyAssertion(URI.create(Vocabulary.P_HAS_A), false);
        sut.addValueFromAxiom(
                new AxiomImpl<>(NamedResource.create(IDENTIFIER), assertion, new Value<>(NamedResource.create(VALUE))));
        final URI another = Generators.createIndividualIdentifier();
        when(mapperMock.getEntityFromCacheOrOntology(eq(OWLClassA.class), eq(another), any())).thenReturn(null);
        sut.addValueFromAxiom(new AxiomImpl<>(NamedResource.create(IDENTIFIER), assertion,
                new Value<>(NamedResource.create(another))));
        final OWLClassD instance = new OWLClassD();
        sut.buildInstanceFieldValue(instance);
        assertSame(existing, instance.getOwlClassA());
        verify(mapperMock).getEntityFromCacheOrOntology(eq(OWLClassA.class), eq(another), any());
    }
}

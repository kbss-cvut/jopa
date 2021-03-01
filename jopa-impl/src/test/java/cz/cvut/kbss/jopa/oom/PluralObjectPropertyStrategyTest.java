/**
 * Copyright (C) 2020 Czech Technical University in Prague
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

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassC;
import cz.cvut.kbss.jopa.environment.OWLClassJ;
import cz.cvut.kbss.jopa.environment.Vocabulary;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.descriptors.ObjectPropertyCollectionDescriptor;
import cz.cvut.kbss.ontodriver.model.*;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.net.URI;
import java.util.Collections;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.*;

class PluralObjectPropertyStrategyTest {

    private static final URI ID = Generators.createIndividualIdentifier();

    @Mock
    private EntityMappingHelper mapperMock;

    private final Descriptor descriptor = new EntityDescriptor();

    private MetamodelMocks mocks;

    @BeforeEach
    void setUp() throws Exception {
        MockitoAnnotations.openMocks(this);
        this.mocks = new MetamodelMocks();
    }

    @Test
    void addValueFromAxiomLoadsInstanceAndAddsItIntoAttributeCollection() {
        final PluralObjectPropertyStrategy<?, OWLClassJ> sut = strategy();
        final OWLClassJ instance = new OWLClassJ(ID);
        final URI aReference = Generators.createIndividualIdentifier();
        final OWLClassA aInstance = new OWLClassA(aReference);
        when(mapperMock.getEntityFromCacheOrOntology(eq(OWLClassA.class), eq(aReference), any(Descriptor.class)))
                .thenReturn(aInstance);
        final Axiom<NamedResource> axiom = new AxiomImpl<>(NamedResource.create(ID),
                Assertion.createObjectPropertyAssertion(URI.create(Vocabulary.P_HAS_A), false), new Value<>(
                NamedResource.create(aReference)));

        sut.addValueFromAxiom(axiom);
        sut.buildInstanceFieldValue(instance);
        assertNotNull(instance.getOwlClassA());
        assertEquals(1, instance.getOwlClassA().size());
        assertTrue(instance.getOwlClassA().contains(aInstance));
        verify(mapperMock).getEntityFromCacheOrOntology(eq(OWLClassA.class), eq(aReference), any(Descriptor.class));
    }

    private PluralObjectPropertyStrategy<?, OWLClassJ> strategy() {
        return new SimpleSetPropertyStrategy<>(mocks.forOwlClassJ().entityType(), mocks.forOwlClassJ().setAttribute(),
                descriptor, mapperMock);
    }

    @Test
    void addValueFromAxiomDoesNothingWhenInstanceCannotBeLoaded() {
        final PluralObjectPropertyStrategy<?, OWLClassJ> sut = strategy();
        final OWLClassJ instance = new OWLClassJ(ID);
        final URI aReference = Generators.createIndividualIdentifier();
        when(mapperMock.getEntityFromCacheOrOntology(eq(OWLClassA.class), eq(aReference), any(Descriptor.class)))
                .thenReturn(null);
        final Axiom<NamedResource> axiom = new AxiomImpl<>(NamedResource.create(ID),
                Assertion.createObjectPropertyAssertion(URI.create(Vocabulary.P_HAS_A), false), new Value<>(
                NamedResource.create(aReference)));

        sut.addValueFromAxiom(axiom);
        sut.buildInstanceFieldValue(instance);
        assertNull(instance.getOwlClassA());
        verify(mapperMock).getEntityFromCacheOrOntology(eq(OWLClassA.class), eq(aReference), any(Descriptor.class));
    }

    @Test
    void addValueFromAxiomGetsAttributeDescriptorFromEntityDescriptorForLoading() {
        final EntityDescriptor aDescriptor = new EntityDescriptor(Generators.createIndividualIdentifier());
        descriptor.addAttributeDescriptor(mocks.forOwlClassJ().setAttribute(), aDescriptor);
        final PluralObjectPropertyStrategy<?, OWLClassJ> sut = strategy();
        final URI aReference = Generators.createIndividualIdentifier();
        when(mapperMock.getEntityFromCacheOrOntology(eq(OWLClassA.class), eq(aReference), any(Descriptor.class)))
                .thenReturn(null);
        final Axiom<NamedResource> axiom = new AxiomImpl<>(NamedResource.create(ID),
                Assertion.createObjectPropertyAssertion(URI.create(Vocabulary.P_HAS_A), false), new Value<>(
                NamedResource.create(aReference)));

        sut.addValueFromAxiom(axiom);
        verify(mapperMock).getEntityFromCacheOrOntology(OWLClassA.class, aReference, aDescriptor);
    }

    @Test
    void buildAxiomValuesFromInstanceChecksForReferenceExistenceUsingTargetReferenceContext() throws Exception {
        final Descriptor aDescriptor = new ObjectPropertyCollectionDescriptor(Generators.createIndividualIdentifier(),
                mocks.forOwlClassJ().setAttribute());
        descriptor.addAttributeDescriptor(mocks.forOwlClassJ().setAttribute(), aDescriptor);
        final PluralObjectPropertyStrategy<?, OWLClassJ> sut = strategy();
        final ReferenceSavingResolver resolverMock = mock(ReferenceSavingResolver.class);
        sut.setReferenceSavingResolver(resolverMock);
        final OWLClassJ instance = new OWLClassJ(ID);
        final OWLClassA aInstance = Generators.generateOwlClassAInstance();
        instance.setOwlClassA(Collections.singleton(aInstance));
        sut.buildAxiomValuesFromInstance(instance, new AxiomValueGatherer(NamedResource.create(ID), null));
        verify(resolverMock).shouldSaveReferenceToItem(aInstance, aDescriptor.getContexts());
    }

    @Test
    void buildAxiomValuesFromInstanceChecksForListItemReferenceExistenceUsingTargetReferenceContext() {
        final Descriptor aDescriptor = new ObjectPropertyCollectionDescriptor(Generators.createIndividualIdentifier(),
                mocks.forOwlClassC().simpleListAtt());
        descriptor.addAttributeDescriptor(mocks.forOwlClassC().simpleListAtt(), aDescriptor);
        final SimpleListPropertyStrategy<OWLClassC> sut =
                new SimpleListPropertyStrategy<>(mocks.forOwlClassC().entityType(),
                        mocks.forOwlClassC().simpleListAtt(), descriptor, mapperMock);
        final ReferenceSavingResolver resolverMock = mock(ReferenceSavingResolver.class);
        sut.setReferenceSavingResolver(resolverMock);
        final OWLClassC instance = new OWLClassC(ID);
        final OWLClassA aInstance = Generators.generateOwlClassAInstance();
        instance.setSimpleList(Collections.singletonList(aInstance));
        sut.buildAxiomValuesFromInstance(instance, new AxiomValueGatherer(NamedResource.create(ID), null));
        verify(resolverMock).shouldSaveReferenceToItem(aInstance, aDescriptor.getContexts());
    }
}

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

import cz.cvut.kbss.jopa.environment.Vocabulary;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.model.IRI;
import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty;
import cz.cvut.kbss.jopa.model.annotations.RDFContainer;
import cz.cvut.kbss.jopa.model.annotations.RDFContainerType;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.CollectionType;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.Identifier;
import cz.cvut.kbss.jopa.model.metamodel.RdfContainerAttributeImpl;
import cz.cvut.kbss.jopa.vocabulary.RDF;
import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.Containers;
import cz.cvut.kbss.ontodriver.descriptor.ContainerDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.ContainerValueDescriptor;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.AxiomImpl;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.net.URI;
import java.util.Collection;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class RdfContainerObjectPropertyStrategyTest {


    private static final URI ID = Generators.createIndividualIdentifier();

    @Mock
    private EntityMappingHelper mappingHelper;

    private final Descriptor descriptor = new EntityDescriptor();

    @Test
    void addAxiomValueLoadsContainerContentAndAddsValuesFromIt() throws Exception {
        final EntityType<EntityWithContainer> et = mock(EntityType.class);
        final RdfContainerAttributeImpl<EntityWithContainer, List<URI>, URI> att = mock(RdfContainerAttributeImpl.class);
        final Collection<Axiom<?>> axioms = List.of(
                new AxiomImpl<>(NamedResource.create(ID), Assertion.createDataPropertyAssertion(URI.create(Vocabulary.ATTRIBUTE_BASE + "levels"), false), new Value<>(NamedResource.create("http://example.com/1"))),
                new AxiomImpl<>(NamedResource.create(ID), Assertion.createDataPropertyAssertion(URI.create(Vocabulary.ATTRIBUTE_BASE + "levels"), false), new Value<>(NamedResource.create("http://example.com/2")))
        );
        when(mappingHelper.loadRdfContainer(any(ContainerDescriptor.class))).thenReturn(axioms);
        when(att.getContainerType()).thenReturn(RDFContainerType.ALT);
        when(att.getJavaField()).thenReturn(EntityWithContainer.class.getDeclaredField("levels"));
        when(att.getIRI()).thenReturn(IRI.create(Vocabulary.ATTRIBUTE_BASE + "levels"));
        when(att.getCollectionType()).thenReturn(CollectionType.LIST);
        when(att.getBindableJavaType()).thenReturn(URI.class);
        final RdfContainerObjectPropertyStrategy<EntityWithContainer> sut = new RdfContainerObjectPropertyStrategy<>(et, att, descriptor, mappingHelper);

        sut.addAxiomValue(new AxiomImpl<>(NamedResource.create(ID), Assertion.createObjectPropertyAssertion(URI.create(Vocabulary.ATTRIBUTE_BASE + "levels"), false), new Value<>(NamedResource.create(Generators.createIndividualIdentifier()))));
        final EntityWithContainer entity = new EntityWithContainer();
        sut.buildInstanceFieldValue(entity);
        assertEquals(List.of(URI.create("http://example.com/1"), URI.create("http://example.com/2")), entity.levels);
        final ArgumentCaptor<ContainerDescriptor> captor = ArgumentCaptor.forClass(ContainerDescriptor.class);
        verify(mappingHelper).loadRdfContainer(captor.capture());
        assertEquals(RDF.ALT, captor.getValue().getType().toString());
        assertEquals(ID, captor.getValue().getOwner().getIdentifier());
        assertEquals(Vocabulary.ATTRIBUTE_BASE + "levels", captor.getValue().getProperty().getIdentifier().toString());
        assertNull(captor.getValue().getContext());
    }

    private static class EntityWithContainer {

        @Id
        private URI uri;

        @RDFContainer(type = RDFContainerType.ALT)
        @OWLObjectProperty(iri = Vocabulary.ATTRIBUTE_BASE + "levels")
        private List<URI> levels;
    }

    @Test
    void buildAxiomValuesFromInstanceAddsContainerDescriptorWithListValuesToValueGatherer() throws Exception {
        final EntityType<EntityWithContainer> et = mock(EntityType.class);
        final RdfContainerAttributeImpl<EntityWithContainer, List<URI>, URI> att = mock(RdfContainerAttributeImpl.class);
        when(att.getContainerType()).thenReturn(RDFContainerType.SEQ);
        when(att.getJavaField()).thenReturn(EntityWithContainer.class.getDeclaredField("levels"));
        when(att.getIRI()).thenReturn(IRI.create(Vocabulary.ATTRIBUTE_BASE + "levels"));
        when(att.getCollectionType()).thenReturn(CollectionType.LIST);
        when(att.getBindableJavaType()).thenReturn(URI.class);
        final Identifier idAtt = mock(Identifier.class);
        when(et.getIdentifier()).thenReturn(idAtt);
        when(idAtt.getJavaField()).thenReturn(EntityWithContainer.class.getDeclaredField("uri"));
        final RdfContainerObjectPropertyStrategy<EntityWithContainer> sut = new RdfContainerObjectPropertyStrategy<>(et, att, descriptor, mappingHelper);
        final EntityWithContainer entity = new EntityWithContainer();
        entity.uri = ID;
        entity.levels = List.of(Generators.createIndividualIdentifier(), Generators.createIndividualIdentifier());

        final AxiomValueGatherer valueGatherer = new AxiomValueGatherer(NamedResource.create(ID), null);
        sut.buildAxiomValuesFromInstance(entity, valueGatherer);
        final Connection connectionMock = mock(Connection.class);
        final Containers containersMock = mock(Containers.class);
        when(connectionMock.containers()).thenReturn(containersMock);
        valueGatherer.update(connectionMock);
        final ArgumentCaptor<ContainerValueDescriptor<NamedResource>> captor = ArgumentCaptor.forClass(ContainerValueDescriptor.class);
        verify(containersMock).updateContainer(captor.capture());
        final ContainerValueDescriptor<NamedResource> valueDescriptor = captor.getValue();
        assertEquals(NamedResource.create(ID), valueDescriptor.getOwner());
        assertEquals(Assertion.createObjectPropertyAssertion(att.getIRI()
                                                                .toURI(), false), valueDescriptor.getProperty());
        assertEquals(entity.levels.stream().map(NamedResource::create).toList(), valueDescriptor.getValues());
    }
}

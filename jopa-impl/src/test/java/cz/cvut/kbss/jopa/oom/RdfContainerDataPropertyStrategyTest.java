package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.environment.Vocabulary;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.model.IRI;
import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;
import cz.cvut.kbss.jopa.model.annotations.RDFContainer;
import cz.cvut.kbss.jopa.model.annotations.RDFContainerType;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.BasicTypeImpl;
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
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class RdfContainerDataPropertyStrategyTest {

    private static final URI ID = Generators.createIndividualIdentifier();

    @Mock
    private EntityMappingHelper mappingHelper;

    private final Descriptor descriptor = new EntityDescriptor();

    private MetamodelMocks metamodelMocks;

    @Test
    void addAxiomValueLoadsContainerValuesViaMappingHelper() throws Exception {
        final EntityType<EntityWithContainer> et = mock(EntityType.class);
        final RdfContainerAttributeImpl<EntityWithContainer, List<Integer>, Integer> att = mock(RdfContainerAttributeImpl.class);
        final Collection<Axiom<?>> axioms = List.of(
                new AxiomImpl<>(NamedResource.create(ID), Assertion.createDataPropertyAssertion(URI.create(Vocabulary.ATTRIBUTE_BASE + "numbers"), false), new Value<>(1)),
                new AxiomImpl<>(NamedResource.create(ID), Assertion.createDataPropertyAssertion(URI.create(Vocabulary.ATTRIBUTE_BASE + "numbers"), false), new Value<>(45))
        );
        when(mappingHelper.loadRdfContainer(any(ContainerDescriptor.class))).thenReturn(axioms);
        when(att.getContainerType()).thenReturn(RDFContainerType.SEQ);
        when(att.getJavaField()).thenReturn(EntityWithContainer.class.getDeclaredField("numbers"));
        when(att.getIRI()).thenReturn(IRI.create(Vocabulary.ATTRIBUTE_BASE + "numbers"));
        when(att.getCollectionType()).thenReturn(CollectionType.LIST);
        when(att.getElementType()).thenReturn(BasicTypeImpl.get(Integer.class));
        final RdfContainerDataPropertyStrategy sut = new RdfContainerDataPropertyStrategy(et, att, descriptor, mappingHelper);

        sut.addAxiomValue(new AxiomImpl<>(NamedResource.create(ID), Assertion.createDataPropertyAssertion(URI.create(Vocabulary.ATTRIBUTE_BASE + "numbers"), false), new Value<>(Generators.createIndividualIdentifier())));
        final EntityWithContainer entity = new EntityWithContainer();
        sut.buildInstanceFieldValue(entity);
        assertEquals(List.of(1, 45), entity.numbers);
        final ArgumentCaptor<ContainerDescriptor> captor = ArgumentCaptor.forClass(ContainerDescriptor.class);
        verify(mappingHelper).loadRdfContainer(captor.capture());
        assertEquals(RDF.SEQ, captor.getValue().getType().toString());
        assertEquals(ID, captor.getValue().getOwner().getIdentifier());
        assertEquals(Vocabulary.ATTRIBUTE_BASE + "numbers", captor.getValue().getProperty().getIdentifier().toString());
        assertNull(captor.getValue().getContext());
    }

    private static class EntityWithContainer {

        @Id
        private URI uri;

        @RDFContainer(type = RDFContainerType.SEQ)
        @OWLDataProperty(iri = Vocabulary.ATTRIBUTE_BASE + "numbers")
        private List<Integer> numbers;
    }

    @Test
    void buildAxiomValuesFromInstanceAddsContainerDescriptorWithListValuesToValueGatherer() throws Exception {
        final EntityType<EntityWithContainer> et = mock(EntityType.class);
        final RdfContainerAttributeImpl<EntityWithContainer, List<Integer>, Integer> att = mock(RdfContainerAttributeImpl.class);
        when(att.getContainerType()).thenReturn(RDFContainerType.SEQ);
        when(att.getJavaField()).thenReturn(EntityWithContainer.class.getDeclaredField("numbers"));
        when(att.getIRI()).thenReturn(IRI.create(Vocabulary.ATTRIBUTE_BASE + "numbers"));
        when(att.getCollectionType()).thenReturn(CollectionType.LIST);
        when(att.getElementType()).thenReturn(BasicTypeImpl.get(Integer.class));
        final Identifier idAtt = mock(Identifier.class);
        when(et.getIdentifier()).thenReturn(idAtt);
        when(idAtt.getJavaField()).thenReturn(EntityWithContainer.class.getDeclaredField("uri"));
        final RdfContainerDataPropertyStrategy sut = new RdfContainerDataPropertyStrategy(et, att, descriptor, mappingHelper);
        final EntityWithContainer entity = new EntityWithContainer();
        entity.uri = ID;
        entity.numbers = List.of(Generators.randomInt(), Generators.randomInt());

        final AxiomValueGatherer valueGatherer = new AxiomValueGatherer(NamedResource.create(ID), null);
        sut.buildAxiomValuesFromInstance(entity, valueGatherer);
        final Connection connectionMock = mock(Connection.class);
        final Containers containersMock = mock(Containers.class);
        when(connectionMock.containers()).thenReturn(containersMock);
        valueGatherer.update(connectionMock);
        final ArgumentCaptor<ContainerValueDescriptor<?>> captor = ArgumentCaptor.forClass(ContainerValueDescriptor.class);
        verify(containersMock).updateContainer(captor.capture());
        final ContainerValueDescriptor valueDescriptor = captor.getValue();
        assertEquals(NamedResource.create(ID), valueDescriptor.getOwner());
        assertEquals(Assertion.createDataPropertyAssertion(att.getIRI().toURI(), false), valueDescriptor.getProperty());
        assertEquals(entity.numbers, valueDescriptor.getValues());
    }
}

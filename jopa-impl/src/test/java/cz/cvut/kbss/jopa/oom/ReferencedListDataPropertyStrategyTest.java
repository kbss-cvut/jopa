package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.environment.Vocabulary;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.model.IRI;
import cz.cvut.kbss.jopa.model.SequencesVocabulary;
import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty;
import cz.cvut.kbss.jopa.model.annotations.Sequence;
import cz.cvut.kbss.jopa.model.annotations.SequenceType;
import cz.cvut.kbss.jopa.model.metamodel.BasicTypeImpl;
import cz.cvut.kbss.jopa.model.metamodel.CollectionType;
import cz.cvut.kbss.jopa.model.metamodel.Converters;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.Identifier;
import cz.cvut.kbss.jopa.model.metamodel.ListAttributeImpl;
import cz.cvut.kbss.jopa.proxy.lazy.LazyLoadingListProxy;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;
import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.Lists;
import cz.cvut.kbss.ontodriver.descriptor.ReferencedListDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.ReferencedListValueDescriptor;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.AxiomImpl;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.net.URI;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class ReferencedListDataPropertyStrategyTest extends ListPropertyStrategyTestBase {

    @BeforeEach
    public void setUp() throws Exception {
        super.setUp();
    }

    @Test
    void createListDescriptorUsesDataPropertyForNodeContentPropertyWhenAttributeTypeIsNotEntityOrUri() throws Exception {
        final EntityType<DataPropertyReferencedList> et = mock(EntityType.class);
        final Identifier id = mock(Identifier.class);
        when(id.getJavaField()).thenReturn(DataPropertyReferencedList.class.getDeclaredField("uri"));
        when(et.getIdentifier()).thenReturn(id);
        final ListAttributeImpl<DataPropertyReferencedList, Integer> att = initDataListAttribute();
        final ReferencedListDataPropertyStrategy<DataPropertyReferencedList> sut = new ReferencedListDataPropertyStrategy<>(et, att, descriptor, mapperMock);
        final Axiom<NamedResource> ax = new AxiomImpl<>(NamedResource.create(IDENTIFIER),
                Assertion.createObjectPropertyAssertion(URI.create(Vocabulary.ATTRIBUTE_BASE + "hasDataList"), false),
                new Value<>(NamedResource.create(Generators.createIndividualIdentifier())));
        final ReferencedListDescriptor result = sut.createListDescriptor(ax);
        final Assertion nodeContentAssertion = result.getNodeContent();
        assertEquals(Assertion.AssertionType.DATA_PROPERTY, nodeContentAssertion.getType());
    }

    static class DataPropertyReferencedList {

        @Id
        private URI uri;

        @Sequence(type = SequenceType.referenced)
        @OWLObjectProperty(iri = Vocabulary.ATTRIBUTE_BASE + "hasDataList")
        private List<Integer> list;
    }

    static ListAttributeImpl<DataPropertyReferencedList, Integer> initDataListAttribute() throws Exception {
        final ListAttributeImpl<DataPropertyReferencedList, Integer> att = mock(ListAttributeImpl.class);
        when(att.getElementType()).thenReturn(BasicTypeImpl.get(Integer.class));
        when(att.isAssociation()).thenReturn(false);
        when(att.getIRI()).thenReturn(IRI.create(Vocabulary.ATTRIBUTE_BASE + "hasDataList"));
        when(att.getCollectionType()).thenReturn(CollectionType.LIST);
        when(att.getJavaField()).thenReturn(DataPropertyReferencedList.class.getDeclaredField("list"));
        when(att.getHasNextPropertyIRI()).thenReturn(IRI.create(SequencesVocabulary.s_p_hasNext));
        when(att.getHasContentsPropertyIRI()).thenReturn(IRI.create(SequencesVocabulary.s_p_hasContents));
        when(att.getConverter()).thenReturn(Converters.getDefaultConverter(Integer.class).get());
        return att;
    }

    @Test
    void buildAxiomValuesFromInstanceHandlesDataPropertyValues() throws Exception {
        when(mapperMock.getEntityType(any(Class.class))).thenThrow(IllegalArgumentException.class);
        final EntityType<DataPropertyReferencedList> et = mock(EntityType.class);
        final Identifier id = mock(Identifier.class);
        when(id.getJavaField()).thenReturn(DataPropertyReferencedList.class.getDeclaredField("uri"));
        when(et.getIdentifier()).thenReturn(id);
        final DataPropertyReferencedList instance = new DataPropertyReferencedList();
        instance.uri = IDENTIFIER;
        instance.list = IntStream.range(0, 10).boxed().collect(Collectors.toList());
        final ListAttributeImpl<DataPropertyReferencedList, Integer> att = initDataListAttribute();
        final ReferencedListDataPropertyStrategy<DataPropertyReferencedList> sut = new ReferencedListDataPropertyStrategy<>(et, att, descriptor, mapperMock);
        sut.setReferenceSavingResolver(new ReferenceSavingResolver(mapperMock));

        sut.buildAxiomValuesFromInstance(instance, builder);
        final Connection conn = mock(Connection.class);
        final Lists lists = mock(Lists.class);
        when(conn.lists()).thenReturn(lists);
        builder.persist(conn);
        final ArgumentCaptor<ReferencedListValueDescriptor<Integer>> captor = ArgumentCaptor.forClass(ReferencedListValueDescriptor.class);
        verify(lists).persistReferencedList(captor.capture());
        assertEquals(instance.list, captor.getValue().getValues());
        final Assertion nodeContentAssertion = captor.getValue().getNodeContent();
        assertEquals(Assertion.AssertionType.DATA_PROPERTY, nodeContentAssertion.getType());
    }

    @Test
    void buildInstanceFieldValueSetsInstanceFieldValueToEmptyListWhenNoValuesWereAdded() throws Exception {
        final EntityType<DataPropertyReferencedList> et = mock(EntityType.class);
        final ListAttributeImpl<DataPropertyReferencedList, Integer> att = initDataListAttribute();
        final ReferencedListDataPropertyStrategy<DataPropertyReferencedList> sut = new ReferencedListDataPropertyStrategy<>(et, att, descriptor, mapperMock);
        final DataPropertyReferencedList instance = new DataPropertyReferencedList();
        instance.uri = Generators.createIndividualIdentifier();
        instance.list = new LazyLoadingListProxy<>(instance, att, mock(UnitOfWork.class));
        sut.buildInstanceFieldValue(instance);
        assertNotNull(instance.list);
        assertTrue(instance.list.isEmpty());
    }
}

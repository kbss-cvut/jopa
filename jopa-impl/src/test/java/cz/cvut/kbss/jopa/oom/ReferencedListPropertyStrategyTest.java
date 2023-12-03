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
import cz.cvut.kbss.jopa.environment.OWLClassC;
import cz.cvut.kbss.jopa.environment.OWLClassP;
import cz.cvut.kbss.jopa.environment.OneOfEnum;
import cz.cvut.kbss.jopa.environment.Vocabulary;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.model.IRI;
import cz.cvut.kbss.jopa.model.SequencesVocabulary;
import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty;
import cz.cvut.kbss.jopa.model.annotations.Sequence;
import cz.cvut.kbss.jopa.model.annotations.SequenceType;
import cz.cvut.kbss.jopa.model.metamodel.CollectionType;
import cz.cvut.kbss.jopa.model.metamodel.Converters;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.Identifier;
import cz.cvut.kbss.jopa.model.metamodel.ListAttributeImpl;
import cz.cvut.kbss.jopa.vocabulary.OWL;
import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.Lists;
import cz.cvut.kbss.ontodriver.descriptor.ReferencedListDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.ReferencedListValueDescriptor;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.AxiomImpl;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class ReferencedListPropertyStrategyTest extends ListPropertyStrategyTestBase {

    private static List<OWLClassA> list;

    private ListAttributeImpl<OWLClassC, OWLClassA> refListMock;

    private ReferencedListPropertyStrategy<OWLClassC> strategy;

    @BeforeAll
    static void setUpBeforeClass() {
        list = generateList();
    }

    @BeforeEach
    public void setUp() throws Exception {
        super.setUp();
        this.refListMock = mocks.forOwlClassC().referencedListAtt();
        this.strategy = new ReferencedListPropertyStrategy<>(mocks.forOwlClassC().entityType(), refListMock, descriptor,
                mapperMock);
        strategy.setReferenceSavingResolver(new ReferenceSavingResolver(mapperMock));
    }

    @Test
    void buildsInstanceFieldFromAxiomsIncludingNodes() throws Exception {
        final OWLClassC c = new OWLClassC(IDENTIFIER);
        final List<Axiom<NamedResource>> axioms = initRefListAxioms(true);
        when(mapperMock.loadReferencedList(any(ReferencedListDescriptor.class))).thenReturn(axioms);
        strategy.addValueFromAxiom(axioms.iterator().next());
        assertNull(c.getReferencedList());
        strategy.buildInstanceFieldValue(c);

        assertNotNull(c.getReferencedList());
        assertEquals(list, c.getReferencedList());
        final ArgumentCaptor<ReferencedListDescriptor> captor = ArgumentCaptor.forClass(ReferencedListDescriptor.class);
        verify(mapperMock).loadReferencedList(captor.capture());
        final ReferencedListDescriptor listDescriptor = captor.getValue();
        assertEquals(IDENTIFIER, listDescriptor.getListOwner().getIdentifier());
        assertEquals(refListMock.getIRI().toURI(), listDescriptor.getListProperty().getIdentifier());
        assertEquals(refListMock.getOWLObjectPropertyHasNextIRI().toURI(), listDescriptor.getNextNode()
                                                                                         .getIdentifier());
        assertEquals(refListMock.getOWLPropertyHasContentsIRI().toURI(), listDescriptor
                .getNodeContent().getIdentifier());
    }

    private List<Axiom<NamedResource>> initRefListAxioms(boolean includeNodes) throws Exception {
        final List<Axiom<NamedResource>> axioms = new ArrayList<>();
        NamedResource previous = NamedResource.create(IDENTIFIER);
        int i = 0;
        for (OWLClassA a : list) {
            final NamedResource nodeUri =
                    NamedResource.create("http://krizik.felk.cvut.cz/ontologies/jopa/refListNode_" + i);
            if (includeNodes) {
                final Axiom<NamedResource> node;
                if (i == 0) {
                    node = new AxiomImpl<>(previous, Assertion.createObjectPropertyAssertion(
                            URI.create(OWLClassC.getRefListField().getAnnotation(OWLObjectProperty.class).iri()),
                            refListMock.isInferred()), new Value<>(nodeUri));
                } else {
                    node = new AxiomImpl<>(
                            previous,
                            Assertion.createObjectPropertyAssertion(refListMock
                                            .getOWLObjectPropertyHasNextIRI().toURI(),
                                    refListMock.isInferred()),
                            new Value<>(nodeUri));
                }
                axioms.add(node);
            }
            final Axiom<NamedResource> content = new AxiomImpl<>(nodeUri,
                    Assertion.createObjectPropertyAssertion(
                            refListMock.getOWLPropertyHasContentsIRI()
                                       .toURI(), refListMock.isInferred()),
                    new Value<>(NamedResource.create(a.getUri())));
            when(mapperMock.getEntityFromCacheOrOntology(OWLClassA.class, a.getUri(),
                    descriptor.getAttributeDescriptor(refListMock))).thenReturn(a);
            axioms.add(content);
            previous = nodeUri;
            i++;
        }
        return axioms;
    }

    /**
     * No node axioms, only content axioms.
     */
    @Test
    void buildsInstanceFieldFromAxiomsWithoutNodes() throws Exception {
        final OWLClassC c = new OWLClassC(IDENTIFIER);
        final List<Axiom<NamedResource>> axioms = initRefListAxioms(false);
        when(mapperMock.loadReferencedList(any(ReferencedListDescriptor.class))).thenReturn(axioms);
        strategy.addValueFromAxiom(axioms.iterator().next());
        assertNull(c.getReferencedList());
        strategy.buildInstanceFieldValue(c);

        assertNotNull(c.getReferencedList());
        assertEquals(list, c.getReferencedList());
    }

    @Test
    void buildsInstanceFieldWithPlainIdentifiers() throws Exception {
        final ListAttributeImpl<OWLClassP, URI> listAtt = mocks.forOwlClassP().pReferencedListAttribute();
        final ReferencedListPropertyStrategy<OWLClassP> strategy =
                new ReferencedListPropertyStrategy<>(mocks.forOwlClassP().entityType(), listAtt, descriptor,
                        mapperMock);
        final List<Axiom<NamedResource>> axioms = initRefListAxioms(true);
        when(mapperMock.loadReferencedList(any(ReferencedListDescriptor.class))).thenReturn(axioms);

        strategy.addValueFromAxiom(axioms.iterator().next());
        final OWLClassP p = new OWLClassP();
        p.setUri(IDENTIFIER);
        strategy.buildInstanceFieldValue(p);
        assertNotNull(p.getReferencedList());
        assertEquals(list.size(), p.getReferencedList().size());
        for (int i = 0; i < list.size(); i++) {
            assertEquals(list.get(i).getUri(), p.getReferencedList().get(i));
        }
    }

    @Test
    void extractsValuesIntoAxiomsForSave() throws Exception {
        when(mapperMock.isManagedType(OWLClassA.class)).thenReturn(true);
        final OWLClassC c = new OWLClassC(IDENTIFIER);
        c.setReferencedList(list);
        strategy.buildAxiomValuesFromInstance(c, builder);

        final ReferencedListValueDescriptor res = listValueDescriptor();
        assertEquals(res.getListOwner(), NamedResource.create(IDENTIFIER));
        assertEquals(
                res.getListProperty(),
                Assertion.createObjectPropertyAssertion(
                        URI.create(OWLClassC.getRefListField().getAnnotation(OWLObjectProperty.class).iri()),
                        refListMock.isInferred()));
        assertEquals(res.getNextNode(), Assertion.createObjectPropertyAssertion(refListMock
                        .getOWLObjectPropertyHasNextIRI()
                        .toURI(),
                refListMock.isInferred()));
        assertEquals(res.getNodeContent(), Assertion.createObjectPropertyAssertion(refListMock
                        .getOWLPropertyHasContentsIRI()
                        .toURI(),
                refListMock.isInferred()));
        final List<URI> expected = list.stream().map(OWLClassA::getUri).collect(Collectors.toList());
        verifyListItems(expected, res);
    }

    private ReferencedListValueDescriptor listValueDescriptor() throws Exception {
        final List<ReferencedListValueDescriptor> descriptors = OOMTestUtils.getReferencedListValueDescriptors(builder);
        assertEquals(1, descriptors.size());
        return descriptors.get(0);
    }

    @Test
    void extractValuesSkipsNullItems() throws Exception {
        final OWLClassC c = new OWLClassC(IDENTIFIER);
        c.setReferencedList(generateList());
        setRandomListItemsToNull(c.getReferencedList());

        strategy.buildAxiomValuesFromInstance(c, builder);
        final ReferencedListValueDescriptor res = listValueDescriptor();
        final List<URI> expected = c.getReferencedList().stream().filter(Objects::nonNull).map(OWLClassA::getUri)
                                    .collect(Collectors.toList());
        verifyListItems(expected, res);
    }

    @Test
    void extractsValuesIntoAxiomsForSaveFromEmptyList() throws Exception {
        final OWLClassC c = new OWLClassC(IDENTIFIER);
        c.setReferencedList(Collections.emptyList());
        strategy.buildAxiomValuesFromInstance(c, builder);

        final ReferencedListValueDescriptor res = listValueDescriptor();
        assertTrue(res.getValues().isEmpty());
    }

    @Test
    void extractsValuesIntoAxiomsForSaveFromNullList() throws Exception {
        final OWLClassC c = new OWLClassC(IDENTIFIER);
        c.setReferencedList(null);
        strategy.buildAxiomValuesFromInstance(c, builder);

        final ReferencedListValueDescriptor res = listValueDescriptor();
        assertTrue(res.getValues().isEmpty());
    }

    @Test
    void extractsValuesIntoAxiomsFromListOfPlainIdentifiers() throws Exception {
        final ListAttributeImpl<OWLClassP, URI> listAtt = mocks.forOwlClassP().pReferencedListAttribute();
        final ReferencedListPropertyStrategy<OWLClassP> strategy =
                new ReferencedListPropertyStrategy<>(mocks.forOwlClassP().entityType(), listAtt, descriptor,
                        mapperMock);
        final OWLClassP p = new OWLClassP();
        p.setUri(IDENTIFIER);
        p.setReferencedList(list.stream().map(OWLClassA::getUri).collect(Collectors.toList()));
        strategy.buildAxiomValuesFromInstance(p, builder);

        final ReferencedListValueDescriptor res = listValueDescriptor();
        p.getReferencedList().forEach(uri -> assertTrue(res.getValues().contains(NamedResource.create(uri))));
    }

    @Test
    void extractValuesFromListSkipsNullItemsInListOfPlainIdentifiers() throws Exception {
        final OWLClassP p = new OWLClassP();
        p.setUri(IDENTIFIER);
        p.setReferencedList(generateListOfIdentifiers());
        setRandomListItemsToNull(p.getReferencedList());
        final List<URI> nonNulls = p.getReferencedList().stream().filter(Objects::nonNull).collect(Collectors.toList());
        final ListAttributeImpl<OWLClassP, URI> refList = mocks.forOwlClassP().pReferencedListAttribute();
        final ReferencedListPropertyStrategy<OWLClassP> strategy =
                new ReferencedListPropertyStrategy<>(mocks.forOwlClassP().entityType(), refList, descriptor,
                        mapperMock);

        strategy.buildAxiomValuesFromInstance(p, builder);
        final ReferencedListValueDescriptor valueDescriptor = listValueDescriptor();
        verifyListItems(nonNulls, valueDescriptor);
    }

    @Test
    void extractValuesRegistersPendingListItemsWhenListContainsUnpersistedItems() {
        final OWLClassC c = new OWLClassC(IDENTIFIER);
        c.setReferencedList(generateList());
        c.getReferencedList()
         .forEach(a -> when(mapperMock.containsEntity(OWLClassA.class, a.getUri(), descriptor)).thenReturn(false));
        strategy.buildAxiomValuesFromInstance(c, builder);
        c.getReferencedList()
         .forEach(item -> verify(mapperMock).registerPendingListReference(eq(item), any(), eq(c.getReferencedList())));
        verify(builder, never()).addReferencedListValues(any());
    }

    @Test
    void extractListValuesConvertsEnumConstantsToNamedResourcesForEnumValuedObjectProperty() throws Exception {
        final EntityType<WithEnumList> et = mock(EntityType.class);
        final Identifier id = mock(Identifier.class);
        when(id.getJavaField()).thenReturn(WithEnumList.class.getDeclaredField("uri"));
        when(et.getIdentifier()).thenReturn(id);
        final ListAttributeImpl<WithEnumList, OneOfEnum> att = initEnumListAttribute();
        final ReferencedListPropertyStrategy<WithEnumList> sut =
                new ReferencedListPropertyStrategy<>(et, att, descriptor, mapperMock);
        final WithEnumList instance = new WithEnumList();
        instance.uri = IDENTIFIER;
        instance.enumList = Arrays.asList(OneOfEnum.DATATYPE_PROPERTY, OneOfEnum.OBJECT_PROPERTY);

        sut.buildAxiomValuesFromInstance(instance, builder);
        final ReferencedListValueDescriptor valueDescriptor = listValueDescriptor();
        assertEquals(
                Arrays.asList(NamedResource.create(OWL.DATATYPE_PROPERTY), NamedResource.create(OWL.OBJECT_PROPERTY)),
                valueDescriptor.getValues());
    }

    @Test
    void createListDescriptorUsesDataPropertyForNodeContentPropertyWhenAttributeTypeIsNotEntityOrUri() throws Exception {
        final EntityType<DataPropertyReferencedList> et = mock(EntityType.class);
        final Identifier id = mock(Identifier.class);
        when(id.getJavaField()).thenReturn(DataPropertyReferencedList.class.getDeclaredField("uri"));
        when(et.getIdentifier()).thenReturn(id);
        final ListAttributeImpl<DataPropertyReferencedList, Integer> att = initDataListAttribute();
        final ReferencedListPropertyStrategy<DataPropertyReferencedList> sut = new ReferencedListPropertyStrategy<>(et, att, descriptor, mapperMock);
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
        when(att.getBindableJavaType()).thenReturn(Integer.class);
        when(att.isAssociation()).thenReturn(false);
        when(att.getIRI()).thenReturn(IRI.create(Vocabulary.ATTRIBUTE_BASE + "hasDataList"));
        when(att.getCollectionType()).thenReturn(CollectionType.LIST);
        when(att.getJavaField()).thenReturn(DataPropertyReferencedList.class.getDeclaredField("list"));
        when(att.getOWLObjectPropertyHasNextIRI()).thenReturn(IRI.create(SequencesVocabulary.s_p_hasNext));
        when(att.getOWLPropertyHasContentsIRI()).thenReturn(IRI.create(SequencesVocabulary.s_p_hasContents));
        when(att.getConverter()).thenReturn(Converters.getDefaultConverter(Integer.class).get());
        return att;
    }

    @Test
    void createListValueDescriptorUsesDataPropertyForNodeContentPropertyWhenAttributeTypeIsNotEntityOrUri() throws Exception {
        final EntityType<DataPropertyReferencedList> et = mock(EntityType.class);
        final Identifier id = mock(Identifier.class);
        when(id.getJavaField()).thenReturn(DataPropertyReferencedList.class.getDeclaredField("uri"));
        when(et.getIdentifier()).thenReturn(id);
        final ListAttributeImpl<DataPropertyReferencedList, Integer> att = initDataListAttribute();
        final ReferencedListPropertyStrategy<DataPropertyReferencedList> sut = new ReferencedListPropertyStrategy<>(et, att, descriptor, mapperMock);
        final DataPropertyReferencedList owner = new DataPropertyReferencedList();
        owner.uri = Generators.createIndividualIdentifier();
        owner.list = List.of(Generators.randomInt(), Generators.randomInt());
        final ReferencedListValueDescriptor<Integer> result = sut.createListValueDescriptor(owner);
        final Assertion nodeContentAssertion = result.getNodeContent();
        assertEquals(Assertion.AssertionType.DATA_PROPERTY, nodeContentAssertion.getType());
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
        final ReferencedListPropertyStrategy<DataPropertyReferencedList> sut = new ReferencedListPropertyStrategy<>(et, att, descriptor, mapperMock);
        sut.setReferenceSavingResolver(new ReferenceSavingResolver(mapperMock));

        sut.buildAxiomValuesFromInstance(instance, builder);
        final Connection conn = mock(Connection.class);
        final Lists lists = mock(Lists.class);
        when(conn.lists()).thenReturn(lists);
        builder.persist(conn);
        final ArgumentCaptor<ReferencedListValueDescriptor<Integer>> captor = ArgumentCaptor.forClass(ReferencedListValueDescriptor.class);
        verify(lists).persistReferencedList(captor.capture());
        assertEquals(instance.list, captor.getValue().getValues());
    }
}

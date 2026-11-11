/*
 * JOPA
 * Copyright (C) 2026 Czech Technical University in Prague
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
import cz.cvut.kbss.jopa.model.MultilingualString;
import cz.cvut.kbss.jopa.model.SequencesVocabulary;
import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.OWLAnnotationProperty;
import cz.cvut.kbss.jopa.model.annotations.Sequence;
import cz.cvut.kbss.jopa.model.annotations.SequenceType;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.BasicTypeImpl;
import cz.cvut.kbss.jopa.model.metamodel.CollectionType;
import cz.cvut.kbss.jopa.model.metamodel.Converters;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.Identifier;
import cz.cvut.kbss.jopa.model.metamodel.ListAttributeImpl;
import cz.cvut.kbss.jopa.oom.converter.ToLangStringConverter;
import cz.cvut.kbss.ontodriver.descriptor.ReferencedListDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.ReferencedListValueDescriptor;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.AxiomImpl;
import cz.cvut.kbss.ontodriver.model.LangString;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class ReferencedListAnnotationPropertyStrategyTest extends ListPropertyStrategyTestBase {

    private static final String LIST_PROPERTY = Vocabulary.ATTRIBUTE_BASE + "hasAnnotationList";

    @BeforeEach
    public void setUp() throws Exception {
        super.setUp();
    }

    @Test
    void addAxiomValueLoadsListValuesFromAxiomsReturnedByMapper() throws Exception {
        final List<String> expected =
                IntStream.range(0, 5).mapToObj(i -> "Value" + i).collect(Collectors.toList());
        final ListAttributeImpl<AnnotationPropertyReferencedList, String> att =
                initAnnotationListAttribute(String.class, "stringList");
        final ReferencedListAnnotationPropertyStrategy<AnnotationPropertyReferencedList> sut =
                new ReferencedListAnnotationPropertyStrategy<>(mock(EntityType.class), att, descriptor, mapperMock);
        final List<Axiom<?>> axioms = initListAxioms(att, expected.stream().map(Value::new)
                                                                  .collect(Collectors.toList()));
        when(mapperMock.loadReferencedList(any(ReferencedListDescriptor.class))).thenReturn(axioms);

        sut.addAxiomValue(axioms.getFirst());
        final AnnotationPropertyReferencedList instance = new AnnotationPropertyReferencedList();
        sut.buildInstanceFieldValue(instance);
        assertEquals(expected, instance.stringList);
    }

    private static List<Axiom<?>> initListAxioms(ListAttributeImpl<?, ?> att, List<Value<?>> values) {
        final List<Axiom<?>> axioms = new ArrayList<>();
        NamedResource previous = NamedResource.create(IDENTIFIER);
        int i = 0;
        for (final Value<?> value : values) {
            final NamedResource nodeUri =
                    NamedResource.create("http://krizik.felk.cvut.cz/ontologies/jopa/refListNode_" + i);
            if (i == 0) {
                axioms.add(new AxiomImpl<>(previous,
                        Assertion.createAnnotationPropertyAssertion(att.getIRI().toURI(), false),
                        new Value<>(nodeUri)));
            } else {
                axioms.add(new AxiomImpl<>(previous,
                        Assertion.createObjectPropertyAssertion(att.getHasNextPropertyIRI().toURI(), false),
                        new Value<>(nodeUri)));
            }
            axioms.add(new AxiomImpl<>(nodeUri,
                    Assertion.createAnnotationPropertyAssertion(att.getHasContentsPropertyIRI().toURI(), false),
                    value));
            previous = nodeUri;
            i++;
        }
        return axioms;
    }

    private static <T> ListAttributeImpl<AnnotationPropertyReferencedList, T> initAnnotationListAttribute(
            Class<T> elementType, String fieldName) throws Exception {
        final ListAttributeImpl<AnnotationPropertyReferencedList, T> att = mock(ListAttributeImpl.class);
        when(att.getElementType()).thenReturn(BasicTypeImpl.get(elementType));
        when(att.isAssociation()).thenReturn(false);
        when(att.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.ANNOTATION);
        when(att.getIRI()).thenReturn(IRI.create(LIST_PROPERTY));
        when(att.getCollectionType()).thenReturn(CollectionType.LIST);
        doReturn(List.class).when(att).getJavaType();
        final Field field = AnnotationPropertyReferencedList.class.getDeclaredField(fieldName);
        field.trySetAccessible();
        when(att.getJavaField()).thenReturn(field);
        when(att.getHasNextPropertyIRI()).thenReturn(IRI.create(SequencesVocabulary.s_p_hasNext));
        when(att.getHasContentsPropertyIRI()).thenReturn(IRI.create(SequencesVocabulary.s_p_hasContents));
        when(att.getConverter()).thenReturn(Converters.getDefaultConverter(elementType).orElse(null));
        return att;
    }

    static class AnnotationPropertyReferencedList {

        @Id
        private URI uri;

        @Sequence(type = SequenceType.referenced)
        @OWLAnnotationProperty(iri = Vocabulary.ATTRIBUTE_BASE + "hasAnnotationList")
        private List<String> stringList;

        @Sequence(type = SequenceType.referenced)
        @OWLAnnotationProperty(iri = Vocabulary.ATTRIBUTE_BASE + "hasAnnotationList")
        private List<URI> uriList;

        @Sequence(type = SequenceType.referenced)
        @OWLAnnotationProperty(iri = Vocabulary.ATTRIBUTE_BASE + "hasAnnotationList")
        private List<MultilingualString> multilingualStringList;
    }

    @Test
    void addAxiomValueConvertsNamedResourceValuesToUris() throws Exception {
        final List<URI> expected =
                IntStream.range(0, 5).mapToObj(i -> Generators.createIndividualIdentifier())
                         .collect(Collectors.toList());
        final ListAttributeImpl<AnnotationPropertyReferencedList, URI> att =
                initAnnotationListAttribute(URI.class, "uriList");
        final ReferencedListAnnotationPropertyStrategy<AnnotationPropertyReferencedList> sut =
                new ReferencedListAnnotationPropertyStrategy<>(mock(EntityType.class), att, descriptor, mapperMock);
        final List<Axiom<?>> axioms = initListAxioms(att,
                expected.stream().map(u -> new Value<>(NamedResource.create(u))).collect(Collectors.toList()));
        when(mapperMock.loadReferencedList(any(ReferencedListDescriptor.class))).thenReturn(axioms);

        sut.addAxiomValue(axioms.get(0));
        final AnnotationPropertyReferencedList instance = new AnnotationPropertyReferencedList();
        sut.buildInstanceFieldValue(instance);
        assertEquals(expected, instance.uriList);
    }

    @Test
    void addAxiomValueCondensesLangStringsIntoMultilingualStringValues() throws Exception {
        final ListAttributeImpl<AnnotationPropertyReferencedList, MultilingualString> att =
                initAnnotationListAttribute(MultilingualString.class, "multilingualStringList");
        when(att.getConverter()).thenReturn(new ToLangStringConverter());
        final ReferencedListAnnotationPropertyStrategy<AnnotationPropertyReferencedList> sut =
                new ReferencedListAnnotationPropertyStrategy<>(mock(EntityType.class), att, descriptor, mapperMock);
        final List<Axiom<?>> axioms = initListAxioms(att, List.of(
                new Value<>(new LangString("Value", "en")),
                new Value<>(new LangString("Hodnota", "cs")),
                new Value<>(new LangString("Another value", "en"))
        ));
        when(mapperMock.loadReferencedList(any(ReferencedListDescriptor.class))).thenReturn(axioms);

        sut.addAxiomValue(axioms.getFirst());
        final AnnotationPropertyReferencedList instance = new AnnotationPropertyReferencedList();
        sut.buildInstanceFieldValue(instance);
        final MultilingualString expectedOne = MultilingualString.create("Value", "en");
        expectedOne.set("cs", "Hodnota");
        assertEquals(List.of(expectedOne, MultilingualString.create("Another value", "en")),
                instance.multilingualStringList);
    }

    @Test
    void addAxiomValueIgnoresNodeAxiomsWithAssertionDifferentFromHasContents() throws Exception {
        final ListAttributeImpl<AnnotationPropertyReferencedList, String> att =
                initAnnotationListAttribute(String.class, "stringList");
        final ReferencedListAnnotationPropertyStrategy<AnnotationPropertyReferencedList> sut =
                new ReferencedListAnnotationPropertyStrategy<>(mock(EntityType.class), att, descriptor, mapperMock);
        final List<Axiom<?>> axioms = initListAxioms(att, List.of(new Value<>("test")));
        when(mapperMock.loadReferencedList(any(ReferencedListDescriptor.class))).thenReturn(axioms);

        sut.addAxiomValue(axioms.getFirst());
        final AnnotationPropertyReferencedList instance = new AnnotationPropertyReferencedList();
        sut.buildInstanceFieldValue(instance);
        // Only the single content axiom should have been processed, node axioms are skipped
        assertEquals(List.of("test"), instance.stringList);
    }

    @Test
    void buildInstanceFieldValueSetsInstanceFieldValueToEmptyListWhenNoValuesWereAdded() throws Exception {
        final ListAttributeImpl<AnnotationPropertyReferencedList, String> att =
                initAnnotationListAttribute(String.class, "stringList");
        final ReferencedListAnnotationPropertyStrategy<AnnotationPropertyReferencedList> sut =
                new ReferencedListAnnotationPropertyStrategy<>(mock(EntityType.class), att, descriptor, mapperMock);

        final AnnotationPropertyReferencedList instance = new AnnotationPropertyReferencedList();
        sut.buildInstanceFieldValue(instance);
        assertTrue(instance.stringList.isEmpty());
    }

    @Test
    void addAxiomValueBuildsListDescriptorForLoadingListContent() throws Exception {
        final ListAttributeImpl<AnnotationPropertyReferencedList, String> att =
                initAnnotationListAttribute(String.class, "stringList");
        final ReferencedListAnnotationPropertyStrategy<AnnotationPropertyReferencedList> sut =
                new ReferencedListAnnotationPropertyStrategy<>(mock(EntityType.class), att, descriptor, mapperMock);
        final Axiom<NamedResource> head = new AxiomImpl<>(NamedResource.create(IDENTIFIER),
                Assertion.createAnnotationPropertyAssertion(URI.create(LIST_PROPERTY), false),
                new Value<>(NamedResource.create(Generators.createIndividualIdentifier())));
        when(mapperMock.loadReferencedList(any(ReferencedListDescriptor.class))).thenReturn(List.of());

        sut.addAxiomValue(head);
        final ArgumentCaptor<ReferencedListDescriptor> captor = ArgumentCaptor.forClass(ReferencedListDescriptor.class);
        verify(mapperMock).loadReferencedList(captor.capture());
        assertEquals(IDENTIFIER, captor.getValue().getListOwner().getIdentifier());
        assertEquals(LIST_PROPERTY, captor.getValue().getListProperty().getIdentifier().toString());
        assertEquals(SequencesVocabulary.s_p_hasNext, captor.getValue().getNextNode().getIdentifier().toString());
        assertEquals(SequencesVocabulary.s_p_hasContents, captor.getValue().getNodeContent().getIdentifier()
                                                                .toString());
        assertEquals(Assertion.AssertionType.ANNOTATION_PROPERTY, captor.getValue().getNodeContent().getType());
    }

    private EntityType<AnnotationPropertyReferencedList> mockEntityType() throws Exception {
        final EntityType<AnnotationPropertyReferencedList> et = mock(EntityType.class);
        final Identifier id = mock(Identifier.class);
        final Field uriField = AnnotationPropertyReferencedList.class.getDeclaredField("uri");
        uriField.trySetAccessible();
        when(id.getJavaField()).thenReturn(uriField);
        when(et.getIdentifier()).thenReturn(id);
        return et;
    }

    private ReferencedListValueDescriptor<Object> listValueDescriptor() throws Exception {
        final List<ReferencedListValueDescriptor> descriptors =
                OOMTestUtils.getReferencedListValueDescriptors(builder);
        assertEquals(1, descriptors.size());
        return descriptors.getFirst();
    }

    @Test
    void buildAxiomValuesFromInstanceExtractsStringValuesIntoListDescriptor() throws Exception {
        final ListAttributeImpl<AnnotationPropertyReferencedList, String> att =
                initAnnotationListAttribute(String.class, "stringList");
        final ReferencedListAnnotationPropertyStrategy<AnnotationPropertyReferencedList> sut =
                new ReferencedListAnnotationPropertyStrategy<>(mockEntityType(), att, descriptor, mapperMock);
        final AnnotationPropertyReferencedList instance = new AnnotationPropertyReferencedList();
        instance.uri = IDENTIFIER;
        instance.stringList = IntStream.range(0, 5).mapToObj(i -> "Value" + i).collect(Collectors.toList());

        sut.buildAxiomValuesFromInstance(instance, builder);
        final ReferencedListValueDescriptor<Object> res = listValueDescriptor();
        assertEquals(NamedResource.create(IDENTIFIER), res.getListOwner());
        assertEquals(instance.stringList, res.getValues());
    }

    @Test
    void buildAxiomValuesFromInstanceConvertsUrisToNamedResources() throws Exception {
        final ListAttributeImpl<AnnotationPropertyReferencedList, URI> att =
                initAnnotationListAttribute(URI.class, "uriList");
        final ReferencedListAnnotationPropertyStrategy<AnnotationPropertyReferencedList> sut =
                new ReferencedListAnnotationPropertyStrategy<>(mockEntityType(), att, descriptor, mapperMock);
        final AnnotationPropertyReferencedList instance = new AnnotationPropertyReferencedList();
        instance.uri = IDENTIFIER;
        instance.uriList = IntStream.range(0, 5).mapToObj(i -> Generators.createIndividualIdentifier())
                                    .collect(Collectors.toList());

        sut.buildAxiomValuesFromInstance(instance, builder);
        final ReferencedListValueDescriptor<Object> res = listValueDescriptor();
        assertEquals(instance.uriList.stream().map(NamedResource::create)
                                     .collect(Collectors.toList()), res.getValues());
    }

    @Test
    void buildAxiomValuesFromInstanceConvertsMultilingualStringsToLangStringValues() throws Exception {
        final ListAttributeImpl<AnnotationPropertyReferencedList, MultilingualString> att =
                initAnnotationListAttribute(MultilingualString.class, "multilingualStringList");
        when(att.getConverter()).thenReturn(new ToLangStringConverter());
        final ReferencedListAnnotationPropertyStrategy<AnnotationPropertyReferencedList> sut =
                new ReferencedListAnnotationPropertyStrategy<>(mockEntityType(), att, descriptor, mapperMock);
        final AnnotationPropertyReferencedList instance = new AnnotationPropertyReferencedList();
        instance.uri = IDENTIFIER;
        final MultilingualString mlsOne = MultilingualString.create("Value", "en");
        mlsOne.set("cs", "Hodnota");
        final MultilingualString mlsTwo = MultilingualString.create("Simple", MultilingualString.NO_LANG);
        instance.multilingualStringList = List.of(mlsOne, mlsTwo);

        sut.buildAxiomValuesFromInstance(instance, builder);
        final ReferencedListValueDescriptor<Object> res = listValueDescriptor();
        final List<LangString> expected = new ArrayList<>();
        mlsOne.forEach((lang, value) -> expected.add(new LangString(value, MultilingualString.NO_LANG.equals(lang) ? null : lang)));
        mlsTwo.forEach((lang, value) -> expected.add(new LangString(value, MultilingualString.NO_LANG.equals(lang) ? null : lang)));
        assertEquals(expected.size(), res.getValues().size());
        expected.forEach(v -> assertTrue(res.getValues().contains(v)));
    }

    @Test
    void buildAxiomValuesFromInstanceSkipsNullListItems() throws Exception {
        final ListAttributeImpl<AnnotationPropertyReferencedList, String> att =
                initAnnotationListAttribute(String.class, "stringList");
        final ReferencedListAnnotationPropertyStrategy<AnnotationPropertyReferencedList> sut =
                new ReferencedListAnnotationPropertyStrategy<>(mockEntityType(), att, descriptor, mapperMock);
        final AnnotationPropertyReferencedList instance = new AnnotationPropertyReferencedList();
        instance.uri = IDENTIFIER;
        instance.stringList = Arrays.asList("One", null, "Two");

        sut.buildAxiomValuesFromInstance(instance, builder);
        final ReferencedListValueDescriptor<Object> res = listValueDescriptor();
        assertEquals(List.of("One", "Two"), res.getValues());
    }

    @Test
    void buildAxiomValuesFromInstanceCreatesDescriptorWithEmptyValuesForNullList() throws Exception {
        final ListAttributeImpl<AnnotationPropertyReferencedList, String> att =
                initAnnotationListAttribute(String.class, "stringList");
        final ReferencedListAnnotationPropertyStrategy<AnnotationPropertyReferencedList> sut =
                new ReferencedListAnnotationPropertyStrategy<>(mockEntityType(), att, descriptor, mapperMock);
        final AnnotationPropertyReferencedList instance = new AnnotationPropertyReferencedList();
        instance.uri = IDENTIFIER;

        sut.buildAxiomValuesFromInstance(instance, builder);
        final ReferencedListValueDescriptor<Object> res = listValueDescriptor();
        assertTrue(res.getValues().isEmpty());
        assertEquals(NamedResource.create(IDENTIFIER), res.getListOwner());
    }
}

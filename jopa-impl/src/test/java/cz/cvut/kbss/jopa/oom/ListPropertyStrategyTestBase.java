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
import cz.cvut.kbss.jopa.environment.OneOfEnum;
import cz.cvut.kbss.jopa.environment.Vocabulary;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.model.IRI;
import cz.cvut.kbss.jopa.model.SequencesVocabulary;
import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.CollectionType;
import cz.cvut.kbss.jopa.model.metamodel.ListAttributeImpl;
import cz.cvut.kbss.jopa.oom.converter.ObjectOneOfEnumConverter;
import cz.cvut.kbss.ontodriver.descriptor.ListValueDescriptor;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import org.mockito.Mock;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.when;

class ListPropertyStrategyTestBase {

    protected static final URI IDENTIFIER = Generators.createIndividualIdentifier();

    @Mock
    protected ObjectOntologyMapperImpl mapperMock;

    protected MetamodelMocks mocks;
    protected Descriptor descriptor;

    AxiomValueGatherer builder;

    protected void setUp() throws Exception {
        this.mocks = new MetamodelMocks();
        when(mapperMock.getEntityType(OWLClassA.class)).thenReturn(mocks.forOwlClassA().entityType());
        this.descriptor = new EntityDescriptor();
        this.builder =
                spy(new AxiomValueGatherer(NamedResource.create(IDENTIFIER), descriptor.getSingleContext().orElse(null)));
        when(mapperMock.containsEntity(any(), any(), any())).thenReturn(true);
    }

    static List<OWLClassA> generateList() {
        final List<OWLClassA> lst = new ArrayList<>();
        for (int i = 0; i < 10; i++) {
            final OWLClassA a = new OWLClassA(Generators.createIndividualIdentifier());
            lst.add(a);
        }
        return lst;
    }

    static List<URI> generateListOfIdentifiers() {
        return IntStream.range(0, 10).mapToObj(i -> Generators.createIndividualIdentifier())
                        .collect(Collectors.toList());
    }

    void setRandomListItemsToNull(List<?> lst) {
        for (int i = 0; i < lst.size(); i++) {
            if (Generators.randomBoolean()) {
                lst.set(i, null);
            }
        }
    }

    void verifyListItems(List<URI> expected, ListValueDescriptor<NamedResource> actual) {
        assertEquals(expected.size(), actual.getValues().size());
        for (int i = 0; i < expected.size(); i++) {
            assertEquals(expected.get(i), actual.getValues().get(i).getIdentifier());
        }
    }

    static ListAttributeImpl<WithEnumList, OneOfEnum> initEnumListAttribute() throws Exception {
        final ListAttributeImpl<WithEnumList, OneOfEnum> att = mock(ListAttributeImpl.class);
        when(att.getBindableJavaType()).thenReturn(OneOfEnum.class);
        when(att.isAssociation()).thenReturn(true);
        when(att.getIRI()).thenReturn(IRI.create(Vocabulary.p_m_objectOneOfEnumAttribute));
        when(att.getConverter()).thenReturn(new ObjectOneOfEnumConverter<>(OneOfEnum.class));
        when(att.getCollectionType()).thenReturn(CollectionType.LIST);
        when(att.getJavaField()).thenReturn(WithEnumList.class.getDeclaredField("enumList"));
        when(att.getOWLObjectPropertyHasNextIRI()).thenReturn(IRI.create(SequencesVocabulary.s_p_hasNext));
        // For referenced list only
        when(att.getOWLPropertyHasContentsIRI()).thenReturn(IRI.create(SequencesVocabulary.s_p_hasContents));
        return att;
    }

    static class WithEnumList {
        @Id
        URI uri;

        List<OneOfEnum> enumList;
    }
}

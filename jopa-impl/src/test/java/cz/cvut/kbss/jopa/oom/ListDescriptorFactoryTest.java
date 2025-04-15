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
import cz.cvut.kbss.jopa.model.metamodel.ListAttribute;
import cz.cvut.kbss.jopa.vocabulary.RDF;
import cz.cvut.kbss.ontodriver.descriptor.ReferencedListDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.ReferencedListValueDescriptor;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class ListDescriptorFactoryTest {

    private static final String HAS_LIST = Vocabulary.ATTRIBUTE_BASE + "hasList";

    private final NamedResource owner = NamedResource.create(Generators.createIndividualIdentifier());

    @Test
    void createReferencedListDescriptorCreatesDescriptorTerminatedByNilForRDFCollectionListAttribute() {
        final ListAttribute<?, ?> att = mock(ListAttribute.class);
        when(att.isRDFCollection()).thenReturn(true);
        when(att.getHasNextPropertyIRI()).thenReturn(IRI.create(RDF.REST));
        when(att.getHasContentsPropertyIRI()).thenReturn(IRI.create(RDF.FIRST));
        when(att.getIRI()).thenReturn(IRI.create(HAS_LIST));

        final ReferencedListDescriptor result = ListDescriptorFactory.createReferencedListDescriptor(owner, att);
        checkReferencedListDescriptorProperties(result);
        assertTrue(result.isTerminatedByNil());
    }

    private static void checkReferencedListDescriptorProperties(ReferencedListDescriptor desc) {
        assertEquals(HAS_LIST, desc.getListProperty().getIdentifier().toString());
        assertEquals(RDF.FIRST, desc.getNodeContent().getIdentifier().toString());
        assertEquals(RDF.REST, desc.getNextNode().getIdentifier().toString());
    }

    @Test
    void createReferencedListDescriptorCreatesDescriptorNotTerminatedByNilForReferencedListAttribute() {
        final ListAttribute<?, ?> att = mock(ListAttribute.class);
        when(att.isRDFCollection()).thenReturn(false);
        when(att.getHasNextPropertyIRI()).thenReturn(IRI.create(RDF.REST));
        when(att.getHasContentsPropertyIRI()).thenReturn(IRI.create(RDF.FIRST));
        when(att.getIRI()).thenReturn(IRI.create(HAS_LIST));

        final ReferencedListDescriptor result = ListDescriptorFactory.createReferencedListDescriptor(owner, att);
        checkReferencedListDescriptorProperties(result);
        assertFalse(result.isTerminatedByNil());
    }

    @Test
    void createReferencedListValueDescriptorCreatesDescriptorTerminatedByNilForRDFCollectionListAttribute() {
        final ListAttribute<?, ?> att = mock(ListAttribute.class);
        when(att.isRDFCollection()).thenReturn(true);
        when(att.getHasNextPropertyIRI()).thenReturn(IRI.create(RDF.REST));
        when(att.getHasContentsPropertyIRI()).thenReturn(IRI.create(RDF.FIRST));
        when(att.getIRI()).thenReturn(IRI.create(HAS_LIST));

        final ReferencedListValueDescriptor<Integer> result = ListDescriptorFactory.createReferencedListValueDescriptor(owner, att);
        checkReferencedListDescriptorProperties(result);
        assertTrue(result.isTerminatedByNil());
    }

    @Test
    void createReferencedListValueDescriptorCreatesDescriptorNotTerminatedByNilForReferencedListAttribute() {
        final ListAttribute<?, ?> att = mock(ListAttribute.class);
        when(att.isRDFCollection()).thenReturn(false);
        when(att.getHasNextPropertyIRI()).thenReturn(IRI.create(RDF.REST));
        when(att.getHasContentsPropertyIRI()).thenReturn(IRI.create(RDF.FIRST));
        when(att.getIRI()).thenReturn(IRI.create(HAS_LIST));

        final ReferencedListValueDescriptor<Integer> result = ListDescriptorFactory.createReferencedListValueDescriptor(owner, att);
        checkReferencedListDescriptorProperties(result);
        assertFalse(result.isTerminatedByNil());
    }
}

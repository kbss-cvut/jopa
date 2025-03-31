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
package cz.cvut.kbss.ontodriver.jena.list;

import cz.cvut.kbss.ontodriver.descriptor.SimpleListDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.SimpleListDescriptorImpl;
import cz.cvut.kbss.ontodriver.jena.environment.Generator;
import cz.cvut.kbss.ontodriver.jena.exception.ListProcessingException;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.Statement;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.net.URI;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static org.apache.jena.rdf.model.ResourceFactory.createResource;
import static org.apache.jena.rdf.model.ResourceFactory.createStatement;
import static org.apache.jena.rdf.model.ResourceFactory.createTypedLiteral;
import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class SimpleListIteratorTest extends ListIteratorTestBase<SimpleListIterator> {

    @BeforeEach
    public void setUp() {
        super.setUp();
    }

    @Override
    SimpleListIterator iterator() {
        return new SimpleListIterator(descriptor(), connectorMock);
    }

    SimpleListDescriptor descriptor() {
        final NamedResource owner = NamedResource.create(RESOURCE.getURI());
        final Assertion hasList = Assertion.createObjectPropertyAssertion(URI.create(HAS_LIST.getURI()), false);
        final Assertion hasNext = Assertion.createObjectPropertyAssertion(URI.create(HAS_NEXT.getURI()), false);
        return new SimpleListDescriptorImpl(owner, hasList, hasNext);
    }

    @Override
    List<URI> generateList() {
        return testUtil.generateSimpleList();
    }

    @Test
    public void nextReturnsFirstListElement() {
        final List<URI> list = generateList();
        final SimpleListIterator iterator = iterator();
        assertTrue(iterator.hasNext());
        final Axiom<NamedResource> head = iterator.nextAxiom();
        assertNotNull(head);
        assertEquals(NamedResource.create(RESOURCE.getURI()), head.getSubject());
        assertEquals(Assertion.createObjectPropertyAssertion(URI.create(HAS_LIST.getURI()), false),
                head.getAssertion());
        assertEquals(NamedResource.create(list.get(0)), head.getValue().getValue());
    }

    @Test
    public void nextThrowsListProcessingExceptionWhenNodeIsLiteral() {
        final List<URI> list = generateList();
        final String nodeUri = list.get(list.size() - 1).toString();
        when(connectorMock.find(createResource(nodeUri), HAS_NEXT, null, Collections.emptySet())).thenReturn(
                Collections.singletonList(createStatement(createResource(nodeUri), HAS_NEXT, createTypedLiteral(117))));
        final SimpleListIterator iterator = new SimpleListIterator(descriptor(), connectorMock);
        final ListProcessingException ex = assertThrows(ListProcessingException.class, () -> {
            while (iterator.hasNext()) {
                iterator.nextAxiom();
            }
        });
        assertThat(ex.getMessage(),
                containsString("Expected successor of node " + nodeUri + " to be a named resource."));
    }

    @Test
    public void removeWithoutReconnectRemovesLastElementInList() {
        final List<URI> list = generateList();
        final SimpleListIterator iterator = new SimpleListIterator(descriptor(), connectorMock);
        while (iterator.hasNext()) {
            iterator.nextAxiom();
        }
        iterator.removeWithoutReconnect();
        verify(connectorMock).remove(createResource(list.get(list.size() - 2).toString()), HAS_NEXT,
                createResource(list.get(list.size() - 1).toString()), null);
    }

    @Test
    public void removeWithoutReconnectRemovesFirstElementInList() {
        final List<URI> list = generateList();
        final SimpleListIterator iterator = new SimpleListIterator(descriptor(), connectorMock);
        iterator.nextValue();
        iterator.removeWithoutReconnect();
        verify(connectorMock).remove(RESOURCE, HAS_LIST, createResource(list.get(0).toString()), null);
    }

    @Test
    public void replaceConnectsNodeToListEnd() {
        final List<URI> list = generateList();
        final SimpleListIterator iterator = new SimpleListIterator(descriptor(), connectorMock);
        while (iterator.hasNext()) {
            iterator.nextValue();
        }
        final NamedResource newNode = NamedResource.create(Generator.generateUri());
        final Resource newOne = createResource(newNode.toString());
        iterator.replace(newNode);
        verify(connectorMock).remove(createResource(list.get(list.size() - 2).toString()), HAS_NEXT,
                createResource(list.get(list.size() - 1).toString()), null);
        final Statement expectedAdded = createStatement(createResource(list.get(list.size() - 2).toString()), HAS_NEXT,
                newOne);
        verify(connectorMock).add(Collections.singletonList(expectedAdded), null);
    }

    @Test
    public void replaceReplacesNodeInsideList() {
        final List<URI> list = generateList();
        final SimpleListIterator iterator = new SimpleListIterator(descriptor(), connectorMock);
        int index = Generator.randomInt(list.size() - 1);
        if (index == 0) {
            index = index + 1;
        }
        int i = 0;
        while (iterator.hasNext() && i <= index) {
            iterator.nextValue();
            i++;
        }
        final NamedResource newNode = NamedResource.create(Generator.generateUri());
        final Resource newOne = createResource(newNode.toString());
        iterator.replace(newNode);
        verify(connectorMock).remove(createResource(list.get(index - 1).toString()), HAS_NEXT,
                createResource(list.get(index).toString()), null);
        verify(connectorMock).remove(createResource(list.get(index).toString()), HAS_NEXT,
                createResource(list.get(index + 1).toString()), null);
        final Statement expectedAddedPrevLink = createStatement(createResource(list.get(index - 1).toString()),
                HAS_NEXT,
                newOne);
        final Statement expectedAddedNextLink = createStatement(newOne, HAS_NEXT,
                createResource(list.get(index + 1).toString()));
        verify(connectorMock).add(Arrays.asList(expectedAddedPrevLink, expectedAddedNextLink), null);
    }

    @Test
    public void replaceReplacesFirstNodeInList() {
        final List<URI> list = generateList();
        final SimpleListIterator iterator = new SimpleListIterator(descriptor(), connectorMock);
        iterator.nextValue();
        final NamedResource newNode = NamedResource.create(Generator.generateUri());
        final Resource newOne = createResource(newNode.toString());
        iterator.replace(newNode);
        verify(connectorMock).remove(RESOURCE, HAS_LIST, createResource(list.get(0).toString()), null);
        verify(connectorMock)
                .remove(createResource(list.get(0).toString()), HAS_NEXT, createResource(list.get(1).toString()), null);
        final Statement expectedAddedPrevLink = createStatement(RESOURCE, HAS_LIST, newOne);
        final Statement expectedAddedNextLink = createStatement(newOne, HAS_NEXT,
                createResource(list.get(1).toString()));
        verify(connectorMock).add(Arrays.asList(expectedAddedPrevLink, expectedAddedNextLink), null);
    }

    @Test
    public void replaceDoesNotConnectNextNodeToItselfWhenListItemIsBeingRemoved() {
        final List<URI> list = generateList();
        final SimpleListIterator iterator = new SimpleListIterator(descriptor(), connectorMock);
        iterator.nextValue();
        iterator.nextValue();
        final NamedResource newNode = NamedResource.create(list.get(2).toString());
        final Resource newOne = createResource(newNode.toString());
        iterator.replace(newNode);
        verify(connectorMock)
                .remove(createResource(list.get(0).toString()), HAS_NEXT, createResource(list.get(1).toString()), null);
        verify(connectorMock)
                .remove(createResource(list.get(1).toString()), HAS_NEXT, createResource(list.get(2).toString()), null);
        final Statement expectedAddedPrevLink = createStatement(createResource(list.get(0).toString()), HAS_NEXT,
                newOne);
        verify(connectorMock).add(Collections.singletonList(expectedAddedPrevLink), null);
    }
}

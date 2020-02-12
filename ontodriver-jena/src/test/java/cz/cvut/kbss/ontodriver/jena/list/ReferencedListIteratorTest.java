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
package cz.cvut.kbss.ontodriver.jena.list;

import cz.cvut.kbss.ontodriver.descriptor.ReferencedListDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.ReferencedListDescriptorImpl;
import cz.cvut.kbss.ontodriver.exception.IntegrityConstraintViolatedException;
import cz.cvut.kbss.ontodriver.jena.environment.Generator;
import cz.cvut.kbss.ontodriver.jena.exception.ListProcessingException;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.Statement;
import org.junit.Before;
import org.junit.Test;
import org.mockito.MockitoAnnotations;

import java.net.URI;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static org.apache.jena.rdf.model.ResourceFactory.*;
import static org.junit.Assert.*;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class ReferencedListIteratorTest extends ListIteratorTestBase<ReferencedListIterator, ReferencedListDescriptor> {

    @Before
    public void setUp() {
        MockitoAnnotations.initMocks(this);
        super.setUp();
        testUtil.setHasContent(HAS_CONTENT);
    }

    @Override
    ReferencedListIterator iterator() {
        return new ReferencedListIterator(descriptor(null), connectorMock);
    }

    @Override
    ReferencedListDescriptor descriptor(String context) {
        final NamedResource owner = NamedResource.create(RESOURCE.getURI());
        final Assertion hasList = Assertion.createObjectPropertyAssertion(URI.create(HAS_LIST.getURI()), false);
        final Assertion hasNext = Assertion.createObjectPropertyAssertion(URI.create(HAS_NEXT.getURI()), false);
        final Assertion hasContent = Assertion.createObjectPropertyAssertion(URI.create(HAS_CONTENT.getURI()), false);
        final ReferencedListDescriptor descriptor = new ReferencedListDescriptorImpl(owner, hasList, hasNext,
                hasContent);
        if (context != null) {
            descriptor.setContext(URI.create(context));
        }
        return descriptor;
    }

    @Override
    List<URI> generateList() {
        return testUtil.generateReferencedList();
    }

    @Test
    public void nextReturnsFirstListElement() {
        final List<URI> list = generateList();
        final AbstractListIterator iterator = iterator();
        assertTrue(iterator.hasNext());
        final Axiom<NamedResource> head = iterator.nextAxiom();
        assertNotNull(head);
        assertEquals(descriptor(null).getNodeContent(), head.getAssertion());
        assertEquals(NamedResource.create(list.get(0)), head.getValue().getValue());
    }

    @Test
    public void nextThrowsListProcessingExceptionWhenNodeIsLiteral() {
        generateList();
        when(connectorMock.find(RESOURCE, HAS_LIST, null, null)).thenReturn(
                Collections.singletonList(createStatement(RESOURCE, HAS_LIST, createTypedLiteral(117))));
        final ReferencedListIterator iterator = iterator();
        thrown.expect(ListProcessingException.class);
        thrown.expectMessage("Expected successor of node " + RESOURCE + " to be a named resource.");
        while (iterator.hasNext()) {
            iterator.nextAxiom();
        }
    }

    @Test
    public void nextThrowsIntegrityConstraintViolationExceptionWhenNodeHasMultipleContentValues() {
        final List<URI> list = generateList();
        final int index = Generator.randomInt(list.size());
        final Resource node = testUtil.getReferencedListNodes().get(index);
        when(connectorMock.find(node, HAS_CONTENT, null, null)).thenReturn(Arrays.asList(
                createStatement(node, HAS_CONTENT, createResource(Generator.generateUri().toString())),
                createStatement(node, HAS_CONTENT, createResource(list.get(index).toString()))
        ));
        final ReferencedListIterator iterator = iterator();
        thrown.expect(IntegrityConstraintViolatedException.class);
        thrown.expectMessage("Encountered multiple content values of list node " + node.getURI());
        while (iterator.hasNext()) {
            iterator.nextAxiom();
        }
    }

    @Test
    public void nextThrowsIntegrityConstraintViolationExceptionWhenNodeHasNoContent() {
        final List<URI> list = generateList();
        final int index = Generator.randomInt(list.size());
        final Resource node = testUtil.getReferencedListNodes().get(index);
        when(connectorMock.find(node, HAS_CONTENT, null, null)).thenReturn(Collections.emptyList());
        final ReferencedListIterator iterator = iterator();
        thrown.expect(IntegrityConstraintViolatedException.class);
        thrown.expectMessage("No content found for list node " + node.getURI());
        while (iterator.hasNext()) {
            iterator.nextAxiom();
        }
    }

    @Test
    public void removeWithoutReconnectRemovesLastElementInList() {
        generateList();
        final ReferencedListIterator iterator = iterator();
        while (iterator.hasNext()) {
            iterator.nextAxiom();
        }
        iterator.removeWithoutReconnect();
        final List<Resource> nodes = testUtil.getReferencedListNodes();
        verify(connectorMock).remove(nodes.get(nodes.size() - 2), HAS_NEXT, nodes.get(nodes.size() - 1), null);
        verify(connectorMock).remove(eq(nodes.get(nodes.size() - 1)), eq(HAS_CONTENT), any(), eq(null));
    }

    @Test
    public void removeWithoutReconnectRemovesFirstElementInList() {
        generateList();
        final ReferencedListIterator iterator = iterator();
        iterator.nextValue();
        iterator.removeWithoutReconnect();
        final List<Resource> nodes = testUtil.getReferencedListNodes();
        verify(connectorMock).remove(RESOURCE, HAS_LIST, nodes.get(0), null);
        verify(connectorMock).remove(eq(nodes.get(0)), eq(HAS_CONTENT), any(), eq(null));
    }

    @Test
    public void replaceReplacesNodeContent() {
        generateList();
        final ReferencedListIterator iterator = iterator();
        iterator.nextValue();
        final Resource replacement = createResource(Generator.generateUri().toString());
        iterator.replace(replacement);

        final List<Resource> nodes = testUtil.getReferencedListNodes();
        verify(connectorMock).remove(nodes.get(0), HAS_CONTENT, null, null);
        final Statement added = createStatement(nodes.get(0), HAS_CONTENT, replacement);
        verify(connectorMock).add(Collections.singletonList(added), null);
    }

    @Test
    public void replaceReplacesNodeContentInContext() {
        final String context = Generator.generateUri().toString();
        testUtil.generateReferencedList(context);
        final ReferencedListIterator iterator = new ReferencedListIterator(descriptor(context), connectorMock);
        iterator.nextValue();
        final Resource replacement = createResource(Generator.generateUri().toString());
        iterator.replace(replacement);

        final List<Resource> nodes = testUtil.getReferencedListNodes();
        verify(connectorMock).remove(nodes.get(0), HAS_CONTENT, null, context);
        final Statement added = createStatement(nodes.get(0), HAS_CONTENT, replacement);
        verify(connectorMock).add(Collections.singletonList(added), context);
    }
}

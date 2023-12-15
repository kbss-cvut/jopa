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
package cz.cvut.kbss.ontodriver.jena.list;

import cz.cvut.kbss.ontodriver.exception.IntegrityConstraintViolatedException;
import cz.cvut.kbss.ontodriver.jena.connector.StorageConnector;
import cz.cvut.kbss.ontodriver.jena.environment.Generator;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.ResourceFactory;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;

import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.NoSuchElementException;

import static org.apache.jena.rdf.model.ResourceFactory.createResource;
import static org.apache.jena.rdf.model.ResourceFactory.createStatement;
import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.when;

public abstract class ListIteratorTestBase<T extends AbstractListIterator<NamedResource, ? extends RDFNode>> {

    static final Resource RESOURCE = createResource(Generator.generateUri().toString());
    static final Property HAS_LIST = ResourceFactory.createProperty(Generator.generateUri().toString());
    static final Property HAS_NEXT = ResourceFactory.createProperty(Generator.generateUri().toString());
    static final Property HAS_CONTENT = ResourceFactory.createProperty(Generator.generateUri().toString());

    @Mock
    StorageConnector connectorMock;

    ListTestUtil testUtil;

    public void setUp() {
        this.testUtil = new ListTestUtil(RESOURCE, HAS_LIST, HAS_NEXT, connectorMock);
    }

    abstract T iterator();

    abstract List<URI> generateList();

    @Test
    public void hasNextReturnsTrueForListHead() {
        generateList();
        final AbstractListIterator<NamedResource, ? extends RDFNode> iterator = iterator();
        assertTrue(iterator.hasNext());
    }

    @Test
    public void nextThrowsNoSuchElementWhenNoMoreElementsExist() {
        final AbstractListIterator<NamedResource, ? extends RDFNode> iterator = iterator();
        assertFalse(iterator.hasNext());
        assertThrows(NoSuchElementException.class, iterator::nextAxiom);
    }

    @Test
    public void nextAllowsToReadWholeList() {
        final List<URI> list = generateList();
        final AbstractListIterator<NamedResource, ? extends RDFNode> iterator = iterator();
        final List<URI> actual = new ArrayList<>();
        while (iterator.hasNext()) {
            final Axiom<NamedResource> axiom = iterator.nextAxiom();
            actual.add(axiom.getValue().getValue().getIdentifier());
        }
        assertEquals(list, actual);
    }

    @Test
    public void nextThrowsIntegrityConstraintViolationWhenMultipleNextNodesAreFound() {
        generateList();
        when(connectorMock.find(RESOURCE, HAS_LIST, null, Collections.emptySet())).thenReturn(
                Arrays.asList(createStatement(RESOURCE, HAS_LIST, createResource()),
                        createStatement(RESOURCE, HAS_LIST, createResource())));
        final AbstractListIterator<NamedResource, ? extends RDFNode> iterator = iterator();
        final IntegrityConstraintViolatedException ex = assertThrows(IntegrityConstraintViolatedException.class,
                iterator::nextAxiom);
        assertThat(ex.getMessage(),
                containsString("Encountered multiple successors of list node " + RESOURCE.getURI()));
    }

    @Test
    public void removeWithoutReconnectThrowsIllegalStateExceptionWhenNextWasNotCalledBefore() {
        generateList();
        final AbstractListIterator<NamedResource, ? extends RDFNode> iterator = iterator();
        final IllegalStateException ex = assertThrows(IllegalStateException.class, iterator::removeWithoutReconnect);
        assertThat(ex.getMessage(), containsString("Cannot call remove before calling next."));
    }

    @Test
    public void removeWithoutReconnectThrowsIllegalStateExceptionWhenRemoveIsCalledTwiceOnElement() {
        generateList();
        final AbstractListIterator<NamedResource, ? extends RDFNode> iterator = iterator();
        iterator.nextValue();
        iterator.removeWithoutReconnect();
        final IllegalStateException ex = assertThrows(IllegalStateException.class, iterator::removeWithoutReconnect);
        assertThat(ex.getMessage(), containsString("Cannot call remove multiple times on one element."));
    }
}

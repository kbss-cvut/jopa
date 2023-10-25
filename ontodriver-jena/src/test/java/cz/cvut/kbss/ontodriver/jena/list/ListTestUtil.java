/*
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.ontodriver.jena.list;

import cz.cvut.kbss.ontodriver.jena.connector.StorageConnector;
import cz.cvut.kbss.ontodriver.jena.environment.Generator;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.Resource;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static org.apache.jena.rdf.model.ResourceFactory.createResource;
import static org.apache.jena.rdf.model.ResourceFactory.createStatement;
import static org.mockito.Mockito.when;

public class ListTestUtil {

    private static final int SIZE = 5;

    private final Resource owner;
    private final Property hasList;
    private final Property hasNext;
    private final StorageConnector connectorMock;

    private Property hasContent;
    private List<Resource> referencedListNodes;

    ListTestUtil(Resource owner, Property hasList, Property hasNext, StorageConnector connectorMock) {
        this.owner = owner;
        this.hasList = hasList;
        this.hasNext = hasNext;
        this.connectorMock = connectorMock;
    }

    void setHasContent(Property hasContent) {
        this.hasContent = hasContent;
    }

    List<URI> generateSimpleList() {
        final List<URI> list = generateList();
        URI previous = null;
        for (final URI node : list) {
            if (previous != null) {
                final Resource prevResource = createResource(previous.toString());
                when(connectorMock.find(prevResource, hasNext, null, Collections.emptySet())).thenReturn(Collections
                        .singletonList(createStatement(prevResource, hasNext, createResource(node.toString()))));
            }
            previous = node;
        }
        when(connectorMock.find(owner, hasList, null, Collections.emptySet())).thenReturn(
                Collections.singletonList(createStatement(owner, hasList, createResource(list.get(0).toString()))));
        return list;
    }

    List<URI> generateList() {
        return IntStream.range(0, SIZE).mapToObj(i -> Generator.generateUri()).collect(Collectors.toList());
    }

    List<URI> generateSimpleList(String context) {
        final List<URI> list = generateList();
        URI previous = null;
        final Collection<String> contexts = context != null ? Collections.singleton(context) : Collections.emptySet();
        for (final URI node : list) {
            if (previous != null) {
                final Resource prevResource = createResource(previous.toString());
                when(connectorMock.find(prevResource, hasNext, null, contexts)).thenReturn(Collections
                        .singletonList(createStatement(prevResource, hasNext, createResource(node.toString()))));
            }
            previous = node;
        }
        when(connectorMock.find(owner, hasList, null, contexts)).thenReturn(
                Collections.singletonList(createStatement(owner, hasList, createResource(list.get(0).toString()))));
        return list;
    }

    List<URI> generateReferencedList() {
        final List<URI> list = generateList();
        this.referencedListNodes = new ArrayList<>(list.size());
        Resource firstNode = null;
        Resource previous = null;
        for (final URI content : list) {
            final Resource node = createResource(Generator.generateUri().toString());
            if (previous != null) {
                final Resource prevResource = createResource(previous.toString());
                when(connectorMock.find(prevResource, hasNext, null, Collections.emptySet())).thenReturn(Collections
                        .singletonList(createStatement(prevResource, hasNext, node)));
            }
            when(connectorMock.find(node, hasContent, null, Collections.emptySet())).thenReturn(
                    Collections.singletonList(createStatement(node, hasContent, createResource(content.toString()))));
            if (firstNode == null) {
                firstNode = node;
            }
            previous = node;
            referencedListNodes.add(node);
        }
        when(connectorMock.find(owner, hasList, null, Collections.emptySet()))
                .thenReturn(Collections.singletonList(createStatement(owner, hasList, firstNode)));
        return list;
    }

    List<URI> generateReferencedList(String context) {
        final List<URI> list = generateList();
        this.referencedListNodes = new ArrayList<>(list.size());
        Resource firstNode = null;
        Resource previous = null;
        final Collection<String> contexts = context != null ? Collections.singleton(context) : Collections.emptySet();
        for (final URI content : list) {
            final Resource node = createResource(Generator.generateUri().toString());
            if (previous != null) {
                final Resource prevResource = createResource(previous.toString());
                when(connectorMock.find(prevResource, hasNext, null, contexts)).thenReturn(Collections
                        .singletonList(createStatement(prevResource, hasNext, node)));
            }
            when(connectorMock.find(node, hasContent, null, contexts)).thenReturn(
                    Collections.singletonList(createStatement(node, hasContent, createResource(content.toString()))));
            if (firstNode == null) {
                firstNode = node;
            }
            previous = node;
            referencedListNodes.add(node);
        }
        when(connectorMock.find(owner, hasList, null, contexts))
                .thenReturn(Collections.singletonList(createStatement(owner, hasList, firstNode)));
        return list;
    }

    public List<Resource> getReferencedListNodes() {
        return referencedListNodes;
    }
}

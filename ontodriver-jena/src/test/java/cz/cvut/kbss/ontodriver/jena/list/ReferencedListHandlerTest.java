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

import cz.cvut.kbss.ontodriver.descriptor.ReferencedListDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.ReferencedListDescriptorImpl;
import cz.cvut.kbss.ontodriver.descriptor.ReferencedListValueDescriptor;
import cz.cvut.kbss.ontodriver.jena.environment.Generator;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.Statement;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static org.apache.jena.rdf.model.ResourceFactory.*;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class ReferencedListHandlerTest
        extends ListHandlerTestBase<ReferencedListDescriptor, ReferencedListValueDescriptor> {

    private static final Assertion HAS_CONTENT = Assertion
            .createObjectPropertyAssertion(Generator.generateUri(), false);
    private static final Property HAS_CONTENT_PROPERTY = createProperty(HAS_CONTENT.getIdentifier().toString());

    @BeforeEach
    public void setUp() {
        this.handler = new ReferencedListHandler(connectorMock);
        super.setUp();
        listUtil.setHasContent(HAS_CONTENT_PROPERTY);
    }

    @Override
    List<URI> generateList(String context) {
        if (context != null) {
            return listUtil.generateReferencedList(context);
        } else {
            return listUtil.generateReferencedList();
        }
    }

    @Override
    ReferencedListDescriptor listDescriptor() {
        return new ReferencedListDescriptorImpl(OWNER, HAS_LIST, HAS_NEXT, HAS_CONTENT);
    }

    @Override
    ReferencedListValueDescriptor listValueDescriptor() {
        return new ReferencedListValueDescriptor(OWNER, HAS_LIST, HAS_NEXT, HAS_CONTENT);
    }

    @Test
    public void persistListInsertsStatementsCorrespondingToList() {
        final List<URI> list = listUtil.generateList();
        final ReferencedListValueDescriptor descriptor = listValueDescriptor();
        list.forEach(item -> descriptor.addValue(NamedResource.create(item)));
        handler.persistList(descriptor);
        final ArgumentCaptor<List> captor = ArgumentCaptor.forClass(List.class);
        verify(connectorMock).add(captor.capture(), eq(null));
        final List<Statement> added = captor.getValue();
        verifyAddedStatements(list, added);
    }

    private void verifyAddedStatements(List<URI> list, List<Statement> added) {
        for (URI value : list) {
            assertTrue(added.stream().anyMatch(s -> s.getObject().asResource().getURI().equals(value.toString()) &&
                    s.getPredicate().equals(HAS_CONTENT_PROPERTY)));
        }
    }

    @Test
    public void persistListWithContextInsertsStatementsCorrespondingToListIntoContext() {
        final List<URI> list = listUtil.generateList();
        final ReferencedListValueDescriptor descriptor = listValueDescriptor();
        list.forEach(item -> descriptor.addValue(NamedResource.create(item)));
        final URI context = Generator.generateUri();
        descriptor.setContext(context);
        handler.persistList(descriptor);
        final ArgumentCaptor<List> captor = ArgumentCaptor.forClass(List.class);
        verify(connectorMock).add(captor.capture(), eq(context.toString()));
        final List<Statement> added = captor.getValue();
        verifyAddedStatements(list, added);
    }

    @Test
    public void updateListAddsNodeWithContentToEnd() {
        final List<URI> list = generateList(null);
        final URI newItem = Generator.generateUri();
        final ReferencedListValueDescriptor descriptor = listValueDescriptor();
        list.forEach(item -> descriptor.addValue(NamedResource.create(item)));
        descriptor.addValue(NamedResource.create(newItem));
        handler.updateList(descriptor);
        final ArgumentCaptor<List> captor = ArgumentCaptor.forClass(List.class);
        verify(connectorMock).add(captor.capture(), eq(null));
        final List<Statement> added = captor.getValue();
        assertEquals(2, added.size());
        final List<Resource> nodes = listUtil.getReferencedListNodes();
        assertEquals(nodes.get(nodes.size() - 1), added.get(0).getSubject());
        assertEquals(HAS_NEXT_PROPERTY, added.get(0).getPredicate());
        assertEquals(HAS_CONTENT_PROPERTY, added.get(1).getPredicate());
        assertEquals(createResource(newItem.toString()), added.get(1).getObject());
    }

    @Test
    public void updateListRemovesRemainingExistingNodes() {
        final List<URI> list = generateList(null);
        final ReferencedListValueDescriptor descriptor =
                new ReferencedListValueDescriptor(OWNER, HAS_LIST, HAS_NEXT, HAS_CONTENT);
        final List<URI> update = list.subList(0, list.size() / 2);
        update.forEach(item -> descriptor.addValue(NamedResource.create(item)));
        handler.updateList(descriptor);

        for (int i = update.size(); i < list.size(); i++) {
            verify(connectorMock).remove(listUtil.getReferencedListNodes().get(i - 1), HAS_NEXT_PROPERTY,
                    listUtil.getReferencedListNodes().get(i), null);
            verify(connectorMock).remove(listUtil.getReferencedListNodes().get(i), HAS_CONTENT_PROPERTY, null, null);
        }
    }

    @Test
    public void updateReplacesModifiedNodeContent() {
        final List<URI> list = generateList(null);
        final ReferencedListValueDescriptor descriptor =
                new ReferencedListValueDescriptor(OWNER, HAS_LIST, HAS_NEXT, HAS_CONTENT);
        final List<URI> update = new ArrayList<>(list);
        final URI replace = Generator.generateUri();
        final int index = Generator.randomInt(list.size());
        update.set(index, replace);
        update.forEach(item -> descriptor.addValue(NamedResource.create(item)));
        handler.updateList(descriptor);

        verify(connectorMock).remove(listUtil.getReferencedListNodes().get(index), HAS_CONTENT_PROPERTY, null, null);
        final Statement added = createStatement(listUtil.getReferencedListNodes().get(index), HAS_CONTENT_PROPERTY,
                createResource(replace.toString()));
        verify(connectorMock).add(Collections.singletonList(added), null);
    }

    @Test
    public void updateClearsOriginalListWhenUpdateIsEmpty() {
        final List<URI> list = generateList(null);
        final ReferencedListValueDescriptor descriptor =
                new ReferencedListValueDescriptor(OWNER, HAS_LIST, HAS_NEXT, HAS_CONTENT);
        handler.updateList(descriptor);

        for (int i = 0; i < list.size(); i++) {
            final Resource node = listUtil.getReferencedListNodes().get(i);
            if (i == 0) {
                verify(connectorMock)
                        .remove(createResource(OWNER.getIdentifier().toString()), HAS_LIST_PROPERTY, node, null);
            } else {
                verify(connectorMock)
                        .remove(listUtil.getReferencedListNodes().get(i - 1), HAS_NEXT_PROPERTY, node, null);
            }
            verify(connectorMock).remove(node, HAS_CONTENT_PROPERTY, null, null);
        }
    }

    @Test
    public void updatePersistsListWhenOriginalWasEmpty() {
        final ReferencedListValueDescriptor descriptor =
                new ReferencedListValueDescriptor(OWNER, HAS_LIST, HAS_NEXT, HAS_CONTENT);
        final List<URI> list = listUtil.generateList();
        list.forEach(u -> descriptor.addValue(NamedResource.create(u)));
        handler.updateList(descriptor);

        final ArgumentCaptor<List> captor = ArgumentCaptor.forClass(List.class);
        verify(connectorMock).add(captor.capture(), eq(null));
        final List<Statement> added = captor.getValue();
        verifyAddedStatements(list, added);
    }

    @Test
    public void updateListWorksInContext() {
        final URI context = Generator.generateUri();
        final List<URI> list = generateList(context.toString());
        final ReferencedListValueDescriptor descriptor =
                new ReferencedListValueDescriptor(OWNER, HAS_LIST, HAS_NEXT, HAS_CONTENT);
        descriptor.setContext(context);
        final URI firstReplaced = Generator.generateUri();
        descriptor.addValue(NamedResource.create(firstReplaced));
        list.subList(1, list.size()).forEach(item -> descriptor.addValue(NamedResource.create(item)));
        final URI added = Generator.generateUri();
        descriptor.addValue(NamedResource.create(added));
        handler.updateList(descriptor);

        final List<Resource> nodes = listUtil.getReferencedListNodes();
        verify(connectorMock)
                .remove(nodes.get(0), HAS_CONTENT_PROPERTY, null, context.toString());
        verify(connectorMock).add(Collections.singletonList(
                createStatement(nodes.get(0), HAS_CONTENT_PROPERTY,
                        createResource(firstReplaced.toString()))), context.toString());
        final ArgumentCaptor<List> captor = ArgumentCaptor.forClass(List.class);
        verify(connectorMock, times(2)).add(captor.capture(), eq(context.toString()));
        final List<Statement> statementsInserted =
                captor.getAllValues().get(1);    // First was the content insert in list head
        assertEquals(nodes.get(nodes.size() - 1), statementsInserted.get(0).getSubject());
        assertEquals(HAS_NEXT_PROPERTY, statementsInserted.get(0).getPredicate());
        assertEquals(statementsInserted.get(0).getObject(), statementsInserted.get(1).getSubject());
        assertEquals(HAS_CONTENT_PROPERTY, statementsInserted.get(1).getPredicate());
        assertEquals(added.toString(), statementsInserted.get(1).getObject().asResource().getURI());
    }
}

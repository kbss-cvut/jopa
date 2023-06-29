/**
 * Copyright (C) 2023 Czech Technical University in Prague
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

import cz.cvut.kbss.ontodriver.descriptor.SimpleListDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.SimpleListDescriptorImpl;
import cz.cvut.kbss.ontodriver.descriptor.SimpleListValueDescriptor;
import cz.cvut.kbss.ontodriver.jena.environment.Generator;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.Statement;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;

import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static org.apache.jena.rdf.model.ResourceFactory.*;
import static org.mockito.Mockito.verify;

@ExtendWith(MockitoExtension.class)
public class SimpleListHandlerTest extends ListHandlerTestBase<SimpleListDescriptor, SimpleListValueDescriptor> {

    @BeforeEach
    public void setUp() {
        this.handler = new SimpleListHandler(connectorMock);
        super.setUp();
    }

    @Override
    List<URI> generateList(String context) {
        if (context != null) {
            return listUtil.generateSimpleList(context);
        } else {
            return listUtil.generateSimpleList();
        }
    }

    @Override
    SimpleListDescriptor listDescriptor() {
        return new SimpleListDescriptorImpl(OWNER, HAS_LIST, HAS_NEXT);
    }

    @Override
    SimpleListValueDescriptor listValueDescriptor() {
        return new SimpleListValueDescriptor(OWNER, HAS_LIST, HAS_NEXT);
    }

    @Test
    public void persistListInsertsStatementsCorrespondingToList() {
        final List<URI> list = listUtil.generateList();
        final SimpleListValueDescriptor descriptor = listValueDescriptor();
        list.forEach(item -> descriptor.addValue(NamedResource.create(item)));
        handler.persistList(descriptor);
        final List<Statement> expected = getExpectedStatementsForPersist(list);
        verify(connectorMock).add(expected, null);
    }

    private List<Statement> getExpectedStatementsForPersist(List<URI> list) {
        final List<Statement> expected = new ArrayList<>(list.size());
        expected.add(createStatement(OWNER_RESOURCE,
                HAS_LIST_PROPERTY, createResource(list.get(0).toString())));
        final Property hasNext = createProperty(HAS_NEXT.getIdentifier().toString());
        for (int i = 0; i < list.size() - 1; i++) {
            expected.add(createStatement(createResource(list.get(i).toString()), hasNext,
                    createResource(list.get(i + 1).toString())));
        }
        return expected;
    }

    @Test
    public void persistListWithContextInsertsStatementsCorrespondingToListIntoContext() {
        final List<URI> list = listUtil.generateList();
        final SimpleListValueDescriptor descriptor = listValueDescriptor();
        list.forEach(item -> descriptor.addValue(NamedResource.create(item)));
        final URI context = Generator.generateUri();
        descriptor.setContext(context);
        handler.persistList(descriptor);
        final List<Statement> expected = getExpectedStatementsForPersist(list);
        verify(connectorMock).add(expected, context.toString());
    }

    @Test
    public void updateListAddsNodeToEnd() {
        final List<URI> list = generateList(null);
        final URI newItem = Generator.generateUri();
        final SimpleListValueDescriptor descriptor = new SimpleListValueDescriptor(OWNER, HAS_LIST, HAS_NEXT);
        list.forEach(item -> descriptor.addValue(NamedResource.create(item)));
        descriptor.addValue(NamedResource.create(newItem));
        handler.updateList(descriptor);
        final Statement expectedAdd = createStatement(createResource(list.get(list.size() - 1).toString()),
                createProperty(HAS_NEXT.getIdentifier().toString()), createResource(newItem.toString()));
        verify(connectorMock).add(Collections.singletonList(expectedAdd), null);
    }

    @Test
    public void updateListRemovesRemainingExistingNodes() {
        final List<URI> list = generateList(null);
        final SimpleListValueDescriptor descriptor = new SimpleListValueDescriptor(OWNER, HAS_LIST, HAS_NEXT);
        final List<URI> update = list.subList(0, list.size() / 2);
        update.forEach(item -> descriptor.addValue(NamedResource.create(item)));
        handler.updateList(descriptor);
        final Property hasNext = createProperty(HAS_NEXT.getIdentifier().toString());
        for (int i = update.size() - 1; i < list.size() - 1; i++) {
            verify(connectorMock).remove(createResource(list.get(i).toString()), hasNext,
                    createResource(list.get(i + 1).toString()), null);
        }
    }

    @Test
    public void updateReplacesModifiedNode() {
        final List<URI> list = generateList(null);
        final SimpleListValueDescriptor descriptor = new SimpleListValueDescriptor(OWNER, HAS_LIST, HAS_NEXT);
        final List<URI> update = new ArrayList<>(list);
        final URI replace = Generator.generateUri();
        final int index = Generator.randomInt(list.size() - 1);
        update.set(index, replace);
        update.forEach(item -> descriptor.addValue(NamedResource.create(item)));
        handler.updateList(descriptor);
        final Resource previous = createResource(
                index > 0 ? list.get(index - 1).toString() : OWNER.getIdentifier().toString());
        final Resource removed = createResource(list.get(index).toString());
        final Resource next = createResource(list.get(index + 1).toString());
        final Property previousLink = index == 0 ? HAS_LIST_PROPERTY : HAS_NEXT_PROPERTY;
        verify(connectorMock).remove(previous, previousLink, removed, null);
        verify(connectorMock).remove(removed, HAS_NEXT_PROPERTY, next, null);
        final List<Statement> expectedAdded = new ArrayList<>(2);
        expectedAdded.add(createStatement(previous, previousLink, createResource(replace.toString())));
        expectedAdded.add(createStatement(createResource(replace.toString()),
                HAS_NEXT_PROPERTY, next));
        verify(connectorMock).add(expectedAdded, null);
    }

    @Test
    public void updateClearsOriginalListWhenUpdateIsEmpty() {
        final List<URI> list = generateList(null);
        final SimpleListValueDescriptor descriptor = new SimpleListValueDescriptor(OWNER, HAS_LIST, HAS_NEXT);
        handler.updateList(descriptor);
        for (int i = 0; i < list.size(); i++) {
            if (i == 0) {
                verify(connectorMock).remove(OWNER_RESOURCE,
                        HAS_LIST_PROPERTY, createResource(list.get(i).toString()), null);
            } else {
                verify(connectorMock).remove(createResource(list.get(i - 1).toString()),
                        HAS_NEXT_PROPERTY, createResource(list.get(i).toString()), null);
            }
        }
    }

    @Test
    public void updatePersistsListWhenOriginalWasEmpty() {
        final List<URI> update = IntStream.range(0, 5).mapToObj(i -> Generator.generateUri())
                                          .collect(Collectors.toList());
        final SimpleListValueDescriptor descriptor = new SimpleListValueDescriptor(OWNER, HAS_LIST, HAS_NEXT);
        update.forEach(v -> descriptor.addValue(NamedResource.create(v)));
        handler.updateList(descriptor);
        final List<Statement> expectedAdded = new ArrayList<>(update.size());
        expectedAdded.add(createStatement(OWNER_RESOURCE, HAS_LIST_PROPERTY, createResource(update.get(0).toString())));
        for (int i = 0; i < update.size() - 1; i++) {
            expectedAdded.add(createStatement(createResource(update.get(i).toString()), HAS_NEXT_PROPERTY,
                    createResource(update.get(i + 1).toString())));
        }
        verify(connectorMock).add(expectedAdded, null);
    }

    @Test
    public void updateListWorksInContext() {
        final URI context = Generator.generateUri();
        final List<URI> list = generateList(context.toString());
        final SimpleListValueDescriptor descriptor = new SimpleListValueDescriptor(OWNER, HAS_LIST, HAS_NEXT);
        descriptor.setContext(context);
        final URI firstReplaced = Generator.generateUri();
        descriptor.addValue(NamedResource.create(firstReplaced));
        list.subList(1, list.size()).forEach(item -> descriptor.addValue(NamedResource.create(item)));
        final URI added = Generator.generateUri();
        descriptor.addValue(NamedResource.create(added));
        handler.updateList(descriptor);
        verify(connectorMock)
                .remove(OWNER_RESOURCE, HAS_LIST_PROPERTY, createResource(list.get(0).toString()), context.toString());
        verify(connectorMock).remove(createResource(list.get(0).toString()), HAS_NEXT_PROPERTY,
                createResource(list.get(1).toString()), context.toString());
        verify(connectorMock).add(Arrays.asList(
                createStatement(OWNER_RESOURCE, HAS_LIST_PROPERTY, createResource(firstReplaced.toString())),
                createStatement(createResource(firstReplaced.toString()), HAS_NEXT_PROPERTY,
                        createResource(list.get(1).toString()))), context.toString());
        verify(connectorMock).add(Collections
                .singletonList(createStatement(createResource(list.get(list.size() - 1).toString()), HAS_NEXT_PROPERTY,
                        createProperty(added.toString()))), context.toString());
    }
}

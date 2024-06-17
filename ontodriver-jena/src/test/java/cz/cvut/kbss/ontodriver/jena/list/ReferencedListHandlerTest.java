/*
 * JOPA
 * Copyright (C) 2024 Czech Technical University in Prague
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
import cz.cvut.kbss.ontodriver.jena.util.JenaUtils;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.LangString;
import cz.cvut.kbss.ontodriver.model.MultilingualString;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.Statement;
import org.apache.jena.vocabulary.RDF;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static org.apache.jena.rdf.model.ResourceFactory.createProperty;
import static org.apache.jena.rdf.model.ResourceFactory.createResource;
import static org.apache.jena.rdf.model.ResourceFactory.createStatement;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasItem;
import static org.hamcrest.Matchers.not;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class ReferencedListHandlerTest extends ListHandlerTestHelper {

    private static final Assertion HAS_CONTENT = Assertion
            .createObjectPropertyAssertion(Generator.generateUri(), false);
    private static final Property HAS_CONTENT_PROPERTY = createProperty(HAS_CONTENT.getIdentifier().toString());

    private ReferencedListHandler sut;

    @BeforeEach
    public void setUp() {
        this.sut = new ReferencedListHandler(connectorMock);
        super.setUp();
        listUtil.setHasContent(HAS_CONTENT_PROPERTY);
    }

    List<URI> generateList(String context) {
        if (context != null) {
            return listUtil.generateReferencedList(context);
        } else {
            return listUtil.generateReferencedList();
        }
    }

    ReferencedListDescriptor listDescriptor() {
        return new ReferencedListDescriptorImpl(OWNER, HAS_LIST, HAS_NEXT, HAS_CONTENT);
    }

    ReferencedListValueDescriptor<NamedResource> listValueDescriptor() {
        return new ReferencedListValueDescriptor<>(OWNER, HAS_LIST, HAS_NEXT, HAS_CONTENT);
    }

    @Test
    public void loadListRetrievesAllListElements() {
        final List<URI> expected = generateList(null);
        final List<Axiom<?>> result = sut.loadList(listDescriptor());
        assertNotNull(result);
        final List<URI> actual = result.stream().map(ax -> URI.create(ax.getValue().getValue().toString()))
                                       .collect(Collectors.toList());
        assertEquals(expected, actual);
    }

    @Test
    public void loadListFromContextRetrievesAllListElements() {
        final URI context = Generator.generateUri();
        final List<URI> expected = generateList(context.toString());
        final ReferencedListDescriptor descriptor = listDescriptor();
        descriptor.setContext(context);
        final List<Axiom<?>> result = sut.loadList(descriptor);
        final List<URI> actual = result.stream().map(ax -> URI.create(ax.getValue().getValue().toString()))
                                       .collect(Collectors.toList());
        assertEquals(expected, actual);
    }

    @Test
    public void persistListDoesNothingForEmptyDescriptor() {
        final ReferencedListValueDescriptor<NamedResource> descriptor = listValueDescriptor();
        sut.persistList(descriptor);
        verify(connectorMock, never()).add(anyList(), anyString());
    }

    @Test
    public void persistListInsertsStatementsCorrespondingToList() {
        final List<URI> list = listUtil.generateList();
        final ReferencedListValueDescriptor<NamedResource> descriptor = listValueDescriptor();
        list.forEach(item -> descriptor.addValue(NamedResource.create(item)));
        sut.persistList(descriptor);
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
        final ReferencedListValueDescriptor<NamedResource> descriptor = listValueDescriptor();
        list.forEach(item -> descriptor.addValue(NamedResource.create(item)));
        final URI context = Generator.generateUri();
        descriptor.setContext(context);
        sut.persistList(descriptor);
        final ArgumentCaptor<List> captor = ArgumentCaptor.forClass(List.class);
        verify(connectorMock).add(captor.capture(), eq(context.toString()));
        final List<Statement> added = captor.getValue();
        verifyAddedStatements(list, added);
    }

    @Test
    public void updateListAddsNodeWithContentToEnd() {
        final List<URI> list = generateList(null);
        final URI newItem = Generator.generateUri();
        final ReferencedListValueDescriptor<NamedResource> descriptor = listValueDescriptor();
        list.forEach(item -> descriptor.addValue(NamedResource.create(item)));
        descriptor.addValue(NamedResource.create(newItem));
        sut.updateList(descriptor);
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
        final ReferencedListValueDescriptor<NamedResource> descriptor =
                new ReferencedListValueDescriptor<>(OWNER, HAS_LIST, HAS_NEXT, HAS_CONTENT);
        final List<URI> update = list.subList(0, list.size() / 2);
        update.forEach(item -> descriptor.addValue(NamedResource.create(item)));
        sut.updateList(descriptor);

        for (int i = update.size(); i < list.size(); i++) {
            verify(connectorMock).remove(listUtil.getReferencedListNodes().get(i - 1), HAS_NEXT_PROPERTY,
                    listUtil.getReferencedListNodes().get(i), null);
            verify(connectorMock).remove(listUtil.getReferencedListNodes().get(i), HAS_CONTENT_PROPERTY, null, null);
        }
    }

    @Test
    public void updateReplacesModifiedNodeContent() {
        final List<URI> list = generateList(null);
        final ReferencedListValueDescriptor<NamedResource> descriptor =
                new ReferencedListValueDescriptor<>(OWNER, HAS_LIST, HAS_NEXT, HAS_CONTENT);
        final List<URI> update = new ArrayList<>(list);
        final URI replace = Generator.generateUri();
        final int index = Generator.randomInt(list.size());
        update.set(index, replace);
        update.forEach(item -> descriptor.addValue(NamedResource.create(item)));
        sut.updateList(descriptor);

        verify(connectorMock).remove(listUtil.getReferencedListNodes().get(index), HAS_CONTENT_PROPERTY, null, null);
        final Statement added = createStatement(listUtil.getReferencedListNodes().get(index), HAS_CONTENT_PROPERTY,
                createResource(replace.toString()));
        verify(connectorMock).add(Collections.singletonList(added), null);
    }

    @Test
    public void updateClearsOriginalListWhenUpdateIsEmpty() {
        final List<URI> list = generateList(null);
        final ReferencedListValueDescriptor<NamedResource> descriptor =
                new ReferencedListValueDescriptor<>(OWNER, HAS_LIST, HAS_NEXT, HAS_CONTENT);
        sut.updateList(descriptor);

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
        final ReferencedListValueDescriptor<NamedResource> descriptor =
                new ReferencedListValueDescriptor<>(OWNER, HAS_LIST, HAS_NEXT, HAS_CONTENT);
        final List<URI> list = listUtil.generateList();
        list.forEach(u -> descriptor.addValue(NamedResource.create(u)));
        sut.updateList(descriptor);

        final ArgumentCaptor<List> captor = ArgumentCaptor.forClass(List.class);
        verify(connectorMock).add(captor.capture(), eq(null));
        final List<Statement> added = captor.getValue();
        verifyAddedStatements(list, added);
    }

    @Test
    public void updateListWorksInContext() {
        final URI context = Generator.generateUri();
        final List<URI> list = generateList(context.toString());
        final ReferencedListValueDescriptor<NamedResource> descriptor =
                new ReferencedListValueDescriptor<>(OWNER, HAS_LIST, HAS_NEXT, HAS_CONTENT);
        descriptor.setContext(context);
        final URI firstReplaced = Generator.generateUri();
        descriptor.addValue(NamedResource.create(firstReplaced));
        list.subList(1, list.size()).forEach(item -> descriptor.addValue(NamedResource.create(item)));
        final URI added = Generator.generateUri();
        descriptor.addValue(NamedResource.create(added));
        sut.updateList(descriptor);

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

    @Test
    void persistListSupportsSavingDataPropertyValuesAsListElements() {
        final ReferencedListValueDescriptor<Integer> desc = new ReferencedListValueDescriptor<>(OWNER,
                HAS_LIST,
                HAS_NEXT, Assertion.createDataPropertyAssertion(HAS_CONTENT.getIdentifier(), false));
        IntStream.range(0, 5).mapToObj(i -> Generator.randomInt()).forEach(desc::addValue);

        sut.persistList(desc);
        final ArgumentCaptor<List> captor = ArgumentCaptor.forClass(List.class);
        verify(connectorMock).add(captor.capture(), eq(null));
        final List<Statement> stmts = captor.getValue();
        assertEquals(desc.getValues().size() * 2, stmts.size());
        int i = 0;
        for (Statement stmt : stmts) {
            if (i == 0) {
                assertEquals(HAS_LIST_PROPERTY, stmt.getPredicate());
            } else if (i % 2 == 1) {
                assertEquals(HAS_CONTENT_PROPERTY, stmt.getPredicate());
                final RDFNode nodeContent = stmt.getObject();
                assertTrue(nodeContent.isLiteral());
                final Integer nodeContentLit = (Integer) JenaUtils.literalToValue(nodeContent.asLiteral());
                assertTrue(desc.getValues().contains(nodeContentLit));
            } else {
                assertEquals(HAS_NEXT_PROPERTY, stmt.getPredicate());
            }
            i++;
        }
    }

    @Test
    public void loadListRetrievesAllDataPropertyListElements() {
        final List<Integer> values = IntStream.range(0, 10).boxed().collect(Collectors.toList());
        listUtil.initReferencedListStatements(values);
        final List<Axiom<?>> result = sut.loadList(listDescriptor());
        assertNotNull(result);
        final List<Integer> actual = result.stream().map(ax -> {
                                               assertInstanceOf(Integer.class, ax.getValue().getValue());
                                               return (Integer) ax.getValue().getValue();
                                           })
                                           .collect(Collectors.toList());
        assertEquals(values, actual);
    }

    @Test
    void persistListSavesMultilingualStringTranslationsAsContentOfSingleNode() {
        final ReferencedListValueDescriptor<MultilingualString> desc = new ReferencedListValueDescriptor<>(OWNER,
                HAS_LIST,
                HAS_NEXT, Assertion.createDataPropertyAssertion(HAS_CONTENT.getIdentifier(), false));
        final List<MultilingualString> refList = List.of(
                new MultilingualString(Map.of("en", "one", "cs", "jedna")),
                new MultilingualString(Map.of("en", "two", "cs", "dva"))
        );
        refList.forEach(desc::addValue);

        sut.persistList(desc);
        final ArgumentCaptor<List> captor = ArgumentCaptor.forClass(List.class);
        verify(connectorMock).add(captor.capture(), eq(null));
        final List<Statement> stmts = captor.getValue();
        assertEquals(desc.getValues().size() * 3, stmts.size());
        int i = 0;
        for (Statement stmt : stmts) {
            if (i == 0) {
                assertEquals(HAS_LIST_PROPERTY, stmt.getPredicate());
            } else if (i % 3 != 0) {
                assertEquals(HAS_CONTENT_PROPERTY, stmt.getPredicate());
                final RDFNode nodeContent = stmt.getObject();
                assertTrue(nodeContent.isLiteral());
                final LangString nodeContentLit = (LangString) JenaUtils.literalToValue(nodeContent.asLiteral());
                assertTrue(nodeContentLit.getLanguage().isPresent());
                final String lang = nodeContentLit.getLanguage().get();
                final String value = nodeContentLit.getValue();
                assertTrue(refList.stream().anyMatch(mls -> mls.getValue().containsKey(lang) && mls.getValue().get(lang)
                                                                                                   .equals(value)));
            } else {
                assertEquals(HAS_NEXT_PROPERTY, stmt.getPredicate());
            }
            i++;
        }
    }

    @Test
    void persistListAppendsRdfNilAfterLastNodeWhenDescriptorIsConfiguredToTerminateListWithIt() {
        final List<URI> values = listUtil.generateList();
        final ReferencedListValueDescriptor<NamedResource> desc = new ReferencedListValueDescriptor<>(OWNER, HAS_LIST, HAS_NEXT, HAS_CONTENT, true);
        values.forEach(v -> desc.addValue(NamedResource.create(v)));

        sut.persistList(desc);
        final ArgumentCaptor<List> captor = ArgumentCaptor.forClass(List.class);
        verify(connectorMock).add(captor.capture(), isNull());
        final Collection<Statement> stmts = captor.getValue();
        final Iterator<Statement> it = stmts.iterator();
        Statement statement = null;
        while (it.hasNext()) {
            statement = it.next();
        }
        assertNotNull(statement);
        assertEquals(HAS_NEXT_PROPERTY, statement.getPredicate());
        assertEquals(RDF.nil, statement.getObject());
    }

    @Test
    void loadListLoadsNilTerminatedList() {
        final ReferencedListDescriptor descriptor = new ReferencedListDescriptorImpl(OWNER, HAS_LIST, HAS_NEXT, HAS_CONTENT, true);
        final List<URI> refList = generateList(null);
        final List<Resource> nodes = listUtil.getReferencedListNodes();
        final Resource lastNodeIri = nodes.get(nodes.size() - 1);
        when(connectorMock.find(lastNodeIri, HAS_NEXT_PROPERTY, null, Collections.emptySet())).thenReturn(
                List.of(createStatement(lastNodeIri, HAS_NEXT_PROPERTY, RDF.nil)));
        final Collection<Axiom<?>> res = sut.loadList(descriptor);
        assertEquals(refList.size(), res.size());
        for (Axiom<?> a : res) {
            assertInstanceOf(NamedResource.class, a.getValue().getValue());
            assertTrue(refList.contains(((NamedResource) a.getValue().getValue()).getIdentifier()));
        }
    }

    @Test
    void clearListRemovesListNodesIncludingNilTerminal() {
        final ReferencedListValueDescriptor<NamedResource> desc = new ReferencedListValueDescriptor<>(OWNER, HAS_LIST, HAS_NEXT, HAS_CONTENT, true);
        // Generate old list
        generateList(null);
        final List<Resource> nodes = listUtil.getReferencedListNodes();
        final Resource lastNodeIri = nodes.get(nodes.size() - 1);
        when(connectorMock.find(lastNodeIri, HAS_NEXT_PROPERTY, null, Collections.emptySet())).thenReturn(
                List.of(createStatement(lastNodeIri, HAS_NEXT_PROPERTY, RDF.nil)));

        sut.updateList(desc);
        final ArgumentCaptor<RDFNode> removedCaptor = ArgumentCaptor.forClass(Resource.class);
        verify(connectorMock, times(nodes.size() * 2 + 1)).remove(any(Resource.class), any(Property.class), removedCaptor.capture(), any());
        verify(connectorMock, never()).add(anyList(), any());
        assertEquals(nodes.size() * 2 + 1, removedCaptor.getAllValues().size());
        nodes.forEach(node -> assertThat(removedCaptor.getAllValues(), hasItem(node)));
        assertThat(removedCaptor.getAllValues(), hasItem(RDF.nil));
    }

    @Test
    void updateListRemovesNilTerminalFromPreviouslyLastNodeAndAppendsItToNewAddedLastNode() {
        final ReferencedListValueDescriptor<NamedResource> desc = new ReferencedListValueDescriptor<>(OWNER, HAS_LIST, HAS_NEXT, HAS_CONTENT, true);
        final List<NamedResource> addedItems = IntStream.range(0, 5)
                                                        .mapToObj(i -> NamedResource.create(Generator.generateUri()))
                                                        .toList();
        final List<URI> oldList = generateList(null);
        final List<Resource> oldNodes = listUtil.getReferencedListNodes();
        final Resource lastNodeIri = oldNodes.get(oldNodes.size() - 1);
        when(connectorMock.find(lastNodeIri, HAS_NEXT_PROPERTY, null, Collections.emptySet()))
                .thenReturn(Set.of(createStatement(lastNodeIri, HAS_NEXT_PROPERTY, RDF.nil)));
        // The original items
        for (URI item : oldList) {
            desc.addValue(NamedResource.create(item));
        }
        // Now add the new ones
        for (NamedResource r : addedItems) {
            desc.addValue(r);
        }

        sut.updateList(desc);
        verify(connectorMock).remove(lastNodeIri, HAS_NEXT_PROPERTY, RDF.nil, null);
        final ArgumentCaptor<List<Statement>> addedCaptor = ArgumentCaptor.forClass(List.class);
        verify(connectorMock, atLeast(1)).add(addedCaptor.capture(), isNull());
        // Added nodes with values + terminal
        assertEquals(addedItems.size() * 2 + 1, addedCaptor.getValue().size());
        for (Statement stmt : addedCaptor.getValue()) {
            if (stmt.getPredicate().equals(HAS_CONTENT_PROPERTY)) {
                final URI u = URI.create(stmt.getObject().asResource().getURI());
                assertTrue(addedItems.contains(NamedResource.create(u)));
            }
        }
        assertTrue(addedCaptor.getValue().stream().anyMatch(s -> s.getObject().equals(RDF.nil)));
    }

    @Test
    void updateListRemovesNilTerminalFromLastNodeWhenItIsRemovedAndAddsItToNewLastNode() {
        final ReferencedListValueDescriptor<NamedResource> desc = new ReferencedListValueDescriptor<>(OWNER, HAS_LIST, HAS_NEXT, HAS_CONTENT, true);
        final List<URI> oldList = generateList(null);
        final List<Resource> oldNodes = listUtil.getReferencedListNodes();
        final Resource lastNode = oldNodes.get(oldNodes.size() - 1);
        when(connectorMock.find(lastNode, HAS_NEXT_PROPERTY, null, Collections.emptySet()))
                .thenReturn(Set.of(createStatement(lastNode, HAS_NEXT_PROPERTY, RDF.nil)));
        // The original items
        for (URI item : oldList.subList(0, oldList.size() / 2)) {
            desc.addValue(NamedResource.create(item));
        }
        sut.updateList(desc);
        verify(connectorMock).remove(lastNode, HAS_NEXT_PROPERTY, RDF.nil, null);
        final ArgumentCaptor<List<Statement>> addedCaptor = ArgumentCaptor.forClass(List.class);
        verify(connectorMock).add(addedCaptor.capture(), isNull());
        assertTrue(addedCaptor.getValue().stream().anyMatch(s -> s.getObject().equals(RDF.nil)));
        assertThat(addedCaptor.getValue(), not(hasItem(createStatement(lastNode, HAS_NEXT_PROPERTY, RDF.nil))));
    }
}

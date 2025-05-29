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
package cz.cvut.kbss.ontodriver.rdf4j.list;

import cz.cvut.kbss.ontodriver.descriptor.ReferencedListDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.ReferencedListDescriptorImpl;
import cz.cvut.kbss.ontodriver.descriptor.ReferencedListValueDescriptor;
import cz.cvut.kbss.ontodriver.exception.IntegrityConstraintViolatedException;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.LangString;
import cz.cvut.kbss.ontodriver.model.Translations;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.rdf4j.connector.RepoConnection;
import cz.cvut.kbss.ontodriver.rdf4j.environment.Generator;
import cz.cvut.kbss.ontodriver.rdf4j.environment.Vocabulary;
import cz.cvut.kbss.ontodriver.rdf4j.util.Rdf4jUtils;
import org.eclipse.rdf4j.model.IRI;
import org.eclipse.rdf4j.model.Literal;
import org.eclipse.rdf4j.model.Resource;
import org.eclipse.rdf4j.model.Statement;
import org.eclipse.rdf4j.model.Value;
import org.eclipse.rdf4j.model.ValueFactory;
import org.eclipse.rdf4j.model.impl.SimpleValueFactory;
import org.eclipse.rdf4j.model.vocabulary.RDF;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
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
import java.util.Random;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static cz.cvut.kbss.ontodriver.rdf4j.list.ListHandlerTestHelper.LIST_PROPERTY;
import static cz.cvut.kbss.ontodriver.rdf4j.list.ListHandlerTestHelper.NEXT_NODE_PROPERTY;
import static cz.cvut.kbss.ontodriver.rdf4j.list.ListHandlerTestHelper.OWNER;
import static cz.cvut.kbss.ontodriver.rdf4j.list.ListHandlerTestHelper.generateList;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasItem;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.anyBoolean;
import static org.mockito.Mockito.anyCollection;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.eq;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@SuppressWarnings({"unchecked", "rawtypes"})
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class ReferencedListHandlerTest {

    private static final ValueFactory vf = SimpleValueFactory.getInstance();
    static final Resource owner = vf.createIRI(OWNER.toString());

    static final IRI hasListProperty = vf.createIRI(LIST_PROPERTY);
    static final IRI nextNodeProperty = vf.createIRI(NEXT_NODE_PROPERTY);
    static final IRI nodeContentProperty = vf.createIRI(ListHandlerTestHelper.NODE_CONTENT_PROPERTY);

    static final Assertion hasListAssertion = Assertion.createObjectPropertyAssertion(URI.create(LIST_PROPERTY), false);
    static final Assertion nextNodeAssertion = Assertion.createObjectPropertyAssertion(URI.create(NEXT_NODE_PROPERTY), false);
    static final Assertion nodeContentAssertion = Assertion.createObjectPropertyAssertion(
            URI.create(ListHandlerTestHelper.NODE_CONTENT_PROPERTY), false);

    @Mock
    private RepoConnection connector;

    private ReferencedListDescriptor listDescriptor;

    private ReferencedListHandler sut;

    private final List<Statement> added = new ArrayList<>();
    private final List<Statement> removed = new ArrayList<>();

    @BeforeEach
    public void setUp() throws Exception {
        this.listDescriptor = new ReferencedListDescriptorImpl(OWNER, hasListAssertion, nextNodeAssertion, nodeContentAssertion);

        this.sut = new ReferencedListHandler(connector, vf);
        doAnswer(invocation -> {
            final Collection<Statement> arg = (Collection<Statement>) invocation.getArguments()[0];
            added.addAll(arg);
            return null;
        }).when(connector).addStatements(anyCollection());
        doAnswer(invocation -> {
            final Collection<Statement> arg = (Collection<Statement>) invocation.getArguments()[0];
            removed.addAll(arg);
            return null;
        }).when(connector).removeStatements(anyCollection());
    }

    @Test
    public void loadsReferencedList() throws Exception {
        final List<NamedResource> refList = generateList();
        final List<URI> listNodes = initListNodes(refList);
        initStatementsForList(listNodes, refList);
        final Collection<Axiom<?>> res = sut.loadList(listDescriptor);
        assertEquals(refList.size(), res.size());
        for (Axiom<?> a : res) {
            assertInstanceOf(NamedResource.class, a.getValue().getValue());
            assertTrue(refList.contains((NamedResource) a.getValue().getValue()));
        }
    }

    private List<URI> initListNodes(List<?> content) {
        final List<URI> nodes = new ArrayList<>();
        for (int i = 0; i < content.size(); i++) {
            nodes.add(URI.create(OWNER + "SEQ_" + i));
        }
        return nodes;
    }

    private List<Statement> initStatementsForList(List<URI> nodes,
                                                  List<?> values) throws Exception {
        int i = 0;
        Resource prev = owner;
        final List<Statement> stmts = new ArrayList<>();
        for (URI item : nodes) {
            final IRI itemUri = vf.createIRI(item.toString());
            Statement node;
            if (i == 0) {
                node = vf.createStatement(prev, hasListProperty, itemUri);
                when(connector.findStatements(eq(prev), eq(hasListProperty), eq(null),
                        anyBoolean(), eq(Collections.emptySet()))).thenReturn(
                        Collections.singleton(node));
            } else {
                node = vf.createStatement(prev, nextNodeProperty, itemUri);
                when(connector.findStatements(eq(prev), eq(nextNodeProperty), eq(null),
                        anyBoolean(), eq(Collections.emptySet()))).thenReturn(
                        Collections.singleton(node));
            }
            stmts.add(node);
            final Object v = values.get(i);
            final Statement content = vf.createStatement(itemUri, nodeContentProperty,
                    v instanceof NamedResource ? vf.createIRI(v.toString()) : Rdf4jUtils.createLiteral(v, null, vf));
            when(connector.findStatements(eq(itemUri), eq(nodeContentProperty),
                    eq(null), anyBoolean(), eq(Collections.emptySet()))).thenReturn(
                    Collections.singleton(content));
            stmts.add(content);
            prev = itemUri;
            i++;
        }
        return stmts;
    }

    @Test
    public void throwsICViolationWhenThereIsNoContentInHeadNode() throws Exception {
        final IRI headNode = vf.createIRI(OWNER + "-SEQ_0");
        when(connector.findStatements(eq(owner), eq(hasListProperty), eq(null),
                anyBoolean(), eq(Collections.emptySet()))).thenReturn(
                Collections.singleton(vf.createStatement(owner, hasListProperty, headNode)));
        when(connector.findStatements(eq(headNode), eq(nodeContentProperty), eq(null),
                anyBoolean(), eq(Collections.emptySet()))).thenReturn(Collections.emptyList());
        assertThrows(IntegrityConstraintViolatedException.class, () -> sut.loadList(listDescriptor));
        verify(connector, never()).findStatements(any(Resource.class), eq(nextNodeProperty),
                any(Value.class), anyBoolean());
    }

    @Test
    public void throwsICViolationWhenThereIsNoContentInSomeListNode() throws Exception {
        final List<NamedResource> refList = generateList();
        final List<URI> listNodes = initListNodes(refList);
        initStatementsForList(listNodes, refList);
        final Resource elem = selectRandomNode(listNodes);
        when(
                connector.findStatements(eq(elem), eq(nodeContentProperty), eq(null),
                        anyBoolean(), eq(Collections.emptySet()))).thenReturn(Collections.emptyList());
        assertThrows(IntegrityConstraintViolatedException.class, () -> sut.loadList(listDescriptor));
    }

    private Resource selectRandomNode(List<URI> nodes) {
        // Select a random index, but it shouldn't be 0 (it would be the head), so add 1
        final int rand = new Random().nextInt(nodes.size() - 1) + 1;
        return vf.createIRI(nodes.get(rand).toString());
    }

    @Test
    public void throwsICViolationWhenThereAreMultipleSuccessorsForNode() throws Exception {
        runIcViolationTest(nextNodeProperty);
    }

    private void runIcViolationTest(IRI property) throws Exception {
        final List<NamedResource> refList = generateList();
        final List<URI> listNodes = initListNodes(refList);
        initStatementsForList(listNodes, refList);
        final Resource node = selectRandomNode(listNodes);
        final List<Statement> stmts = List.of(
                vf.createStatement(node, property, vf.createIRI(Generator.generateUri().toString())),
                vf.createStatement(node, property, vf.createIRI(Generator.generateUri().toString())));
        when(connector.findStatements(eq(node), eq(property), eq(null),
                anyBoolean(), eq(Collections.emptySet()))).thenReturn(stmts);
        assertThrows(IntegrityConstraintViolatedException.class, () -> sut.loadList(listDescriptor));
    }

    @Test
    public void throwsICViolationWhenThereAreMultipleReferencesInNode() throws Exception {
        runIcViolationTest(nodeContentProperty);
    }

    @Test
    public void persistsReferencedList() throws Exception {
        final List<NamedResource> values = generateList();
        final ReferencedListValueDescriptor<NamedResource> valueDescriptor = new ReferencedListValueDescriptor<>(OWNER, hasListAssertion, nextNodeAssertion, nodeContentAssertion);
        for (NamedResource val : values) {
            valueDescriptor.addValue(val);
        }
        final ArgumentCaptor<Collection> captor = ArgumentCaptor.forClass(Collection.class);
        sut.persistList(valueDescriptor);
        verify(connector).addStatements(captor.capture());
        final Collection<Statement> stmts = captor.getValue();
        assertEquals(values.size() * 2, stmts.size());
        int i = 0;
        for (Statement stmt : stmts) {
            if (i == 0) {
                assertEquals(hasListProperty, stmt.getPredicate());
            } else if (i % 2 == 1) {
                assertEquals(nodeContentProperty, stmt.getPredicate());
                assertTrue(values.contains(NamedResource.create(stmt.getObject().stringValue())));
            } else {
                assertEquals(nextNodeProperty, stmt.getPredicate());
            }
            i++;
        }
    }

    @Test
    public void doesNothingWhenNoValuesArePassedToPersist() throws Exception {
        final ReferencedListValueDescriptor<NamedResource> valueDescriptor = new ReferencedListValueDescriptor<>(OWNER, hasListAssertion, nextNodeAssertion, nodeContentAssertion);
        assertTrue(valueDescriptor.getValues().isEmpty());
        final ReferencedListValueDescriptor spiedValues = spy(valueDescriptor);
        sut.persistList(spiedValues);
        verify(spiedValues).getValues();
        verify(connector, never()).addStatements(anyCollection());
    }

    @Test
    public void clearsListOnUpdateWhenDescriptorHasNoValues() throws Exception {
        final ReferencedListValueDescriptor<NamedResource> descriptor = createValueDescriptor(Assertion.AssertionType.OBJECT_PROPERTY, false);
        // old list
        final List<NamedResource> refList = generateList();
        final List<Statement> oldList = initStatementsForList(initListNodes(refList), refList);

        sut.updateList(descriptor);
        verify(connector).removeStatements(anyCollection());
        verify(connector, never()).addStatements(anyCollection());
        assertEquals(oldList.size(), removed.size());
        assertTrue(removed.containsAll(oldList));
    }

    private static ReferencedListValueDescriptor<NamedResource> initValues(int count) {
        final ReferencedListValueDescriptor<NamedResource> desc = createValueDescriptor(Assertion.AssertionType.OBJECT_PROPERTY, false);
        for (int i = 0; i < count; i++) {
            desc.addValue(NamedResource.create(Vocabulary.INDIVIDUAL_IRI_BASE + i));
        }
        return desc;
    }

    private static <T> ReferencedListValueDescriptor<T> createValueDescriptor(Assertion.AssertionType assertionType,
                                                                              boolean nilTerminal) {
        final Assertion contentAssertion = assertionType == Assertion.AssertionType.OBJECT_PROPERTY ? Assertion.createObjectPropertyAssertion(
                URI.create(ListHandlerTestHelper.NODE_CONTENT_PROPERTY), false) : Assertion.createDataPropertyAssertion(URI.create(ListHandlerTestHelper.NODE_CONTENT_PROPERTY), false);
        return new ReferencedListValueDescriptor<>(OWNER,
                Assertion.createObjectPropertyAssertion(URI.create(LIST_PROPERTY), false),
                Assertion.createObjectPropertyAssertion(URI.create(NEXT_NODE_PROPERTY),
                        false), contentAssertion, nilTerminal);
    }

    @Test
    public void insertsListOnUpdateWhenThereWereNoValuesBefore() throws Exception {
        final ReferencedListValueDescriptor<NamedResource> descriptor = initValues(5);
        when(
                connector.findStatements(owner, hasListProperty, null, descriptor.getListProperty().isInferred()))
                .thenReturn(Collections.emptyList());

        sut.updateList(descriptor);
        verify(connector, never()).removeStatements(anyCollection());
        int i = 0;
        assertFalse(added.isEmpty());
        for (Statement stmt : added) {
            if (stmt.getPredicate().equals(nodeContentProperty)) {
                assertEquals(descriptor.getValues().get(i++).getIdentifier(),
                        URI.create(stmt.getObject().stringValue()));
            }
        }
    }

    @Test
    public void updateListAddsNewValuesToTheEnd() throws Exception {
        final ReferencedListValueDescriptor<NamedResource> descriptor = initValues(0);
        final ReferencedListValueDescriptor<NamedResource> tempDesc = initValues(8);
        final List<NamedResource> refList = generateList();
        initStatementsForList(initListNodes(refList), refList);
        // The original items
        for (NamedResource item : refList) {
            descriptor.addValue(item);
        }
        // Now add the new ones
        for (NamedResource r : tempDesc.getValues()) {
            descriptor.addValue(r);
        }

        sut.updateList(descriptor);
        verify(connector, never()).removeStatements(anyCollection());
        verify(connector, atLeast(1)).addStatements(anyCollection());
        assertEquals(tempDesc.getValues().size() * 2, added.size());
        for (Statement stmt : added) {
            if (stmt.getPredicate().equals(nodeContentProperty)) {
                final URI u = URI.create(stmt.getObject().stringValue());
                assertTrue(tempDesc.getValues().contains(NamedResource.create(u)));
            }
        }
    }

    @Test
    void persistListSupportsSavingDataPropertyValuesAsListElements() throws Exception {
        final ReferencedListValueDescriptor<Integer> desc = createValueDescriptor(Assertion.AssertionType.DATA_PROPERTY, false);
        IntStream.range(0, 5).mapToObj(i -> Generator.randomInt()).forEach(desc::addValue);

        sut.persistList(desc);
        final ArgumentCaptor<Collection> captor = ArgumentCaptor.forClass(Collection.class);
        verify(connector).addStatements(captor.capture());
        final Collection<Statement> stmts = captor.getValue();
        assertEquals(desc.getValues().size() * 2, stmts.size());
        int i = 0;
        for (Statement stmt : stmts) {
            if (i == 0) {
                assertEquals(hasListProperty, stmt.getPredicate());
            } else if (i % 2 == 1) {
                assertEquals(nodeContentProperty, stmt.getPredicate());
                final Value nodeContent = stmt.getObject();
                assertTrue(nodeContent.isLiteral());
                final Integer nodeContentLit = (Integer) Rdf4jUtils.getLiteralValue((Literal) nodeContent);
                assertTrue(desc.getValues().contains(nodeContentLit));
            } else {
                assertEquals(nextNodeProperty, stmt.getPredicate());
            }
            i++;
        }
    }

    @Test
    public void loadsReferencedListConsistingOfLiterals() throws Exception {
        final List<Integer> refList = IntStream.range(0, 5).boxed().collect(Collectors.toList());
        final List<URI> listNodes = initListNodes(refList);
        initStatementsForList(listNodes, refList);
        final Collection<Axiom<?>> res = sut.loadList(listDescriptor);
        assertEquals(refList.size(), res.size());
        for (Axiom<?> a : res) {
            assertInstanceOf(Integer.class, a.getValue().getValue());
            assertTrue(refList.contains((Integer) a.getValue().getValue()));
        }
    }

    @Test
    void persistListSavesMultilingualStringTranslationsAsContentOfSingleNode() throws Exception {
        final List<Translations> refList = List.of(
                new Translations(Map.of("en", "one", "cs", "jedna")),
                new Translations(Map.of("en", "two", "cs", "dva"))
        );
        final ReferencedListValueDescriptor<Translations> desc = createValueDescriptor(Assertion.AssertionType.DATA_PROPERTY, false);
        refList.forEach(desc::addValue);

        sut.persistList(desc);
        final ArgumentCaptor<Collection> captor = ArgumentCaptor.forClass(Collection.class);
        verify(connector).addStatements(captor.capture());
        final Collection<Statement> stmts = captor.getValue();
        assertEquals(desc.getValues().size() * 3, stmts.size());
        int i = 0;
        for (Statement stmt : stmts) {
            if (i == 0) {
                assertEquals(hasListProperty, stmt.getPredicate());
            } else if (i % 3 != 0) {
                assertEquals(nodeContentProperty, stmt.getPredicate());
                final Value nodeContent = stmt.getObject();
                assertTrue(nodeContent.isLiteral());
                final LangString nodeContentLit = (LangString) Rdf4jUtils.getLiteralValue((Literal) nodeContent);
                assertTrue(nodeContentLit.getLanguage().isPresent());
                final String lang = nodeContentLit.getLanguage().get();
                final String value = nodeContentLit.getValue();
                assertTrue(refList.stream().anyMatch(mls -> mls.getValue().containsKey(lang) && mls.getValue().get(lang)
                                                                                                   .equals(value)));
            } else {
                assertEquals(nextNodeProperty, stmt.getPredicate());
            }
            i++;
        }
    }

    @Test
    void persistListAppendsRdfNilAfterLastNodeWhenDescriptorIsConfiguredToTerminateListWithIt() throws Exception {
        final List<NamedResource> values = generateList();
        final ReferencedListValueDescriptor<NamedResource> desc = createValueDescriptor(Assertion.AssertionType.OBJECT_PROPERTY, true);
        values.forEach(desc::addValue);

        sut.persistList(desc);
        final ArgumentCaptor<Collection> captor = ArgumentCaptor.forClass(Collection.class);
        verify(connector).addStatements(captor.capture());
        final Collection<Statement> stmts = captor.getValue();
        final Iterator<Statement> it = stmts.iterator();
        Statement statement = null;
        while (it.hasNext()) {
            statement = it.next();
        }
        assertNotNull(statement);
        assertEquals(vf.createIRI(NEXT_NODE_PROPERTY), statement.getPredicate());
        assertEquals(RDF.NIL, statement.getObject());
    }

    @Test
    void loadListLoadsNilTerminatedList() throws Exception {
        final List<NamedResource> refList = generateList();
        final List<URI> listNodes = initListNodes(refList);
        initStatementsForList(listNodes, refList);
        final IRI lastNodeIri = vf.createIRI(listNodes.get(listNodes.size() - 1).toString());
        when(connector.findStatements(lastNodeIri, vf.createIRI(NEXT_NODE_PROPERTY), null, false, Collections.emptySet()))
                .thenReturn(Set.of(vf.createStatement(lastNodeIri, vf.createIRI(NEXT_NODE_PROPERTY), RDF.NIL)));
        final Collection<Axiom<?>> res = sut.loadList(listDescriptor);
        assertEquals(refList.size(), res.size());
        for (Axiom<?> a : res) {
            assertInstanceOf(NamedResource.class, a.getValue().getValue());
            assertTrue(refList.contains((NamedResource) a.getValue().getValue()));
        }
    }

    @Test
    void clearListRemovesListNodesIncludingNilTerminal() throws Exception {
        final ReferencedListValueDescriptor<NamedResource> descriptor = createValueDescriptor(Assertion.AssertionType.OBJECT_PROPERTY, true);
        // old list
        final List<NamedResource> refList = generateList();
        final List<URI> oldListNodes = initListNodes(refList);
        final List<Statement> oldList = initStatementsForList(oldListNodes, refList);
        final IRI lastNodeIri = vf.createIRI(oldListNodes.get(oldListNodes.size() - 1).toString());
        final Statement nilTerminal = vf.createStatement(lastNodeIri, vf.createIRI(NEXT_NODE_PROPERTY), RDF.NIL);
        when(connector.findStatements(lastNodeIri, vf.createIRI(NEXT_NODE_PROPERTY), null, false, Collections.emptySet()))
                .thenReturn(Set.of(nilTerminal));

        sut.updateList(descriptor);
        verify(connector).removeStatements(anyCollection());
        verify(connector, never()).addStatements(anyCollection());
        assertEquals(oldList.size() + 1, removed.size());
        assertTrue(removed.containsAll(oldList));
        assertThat(removed, hasItem(nilTerminal));
    }

    @Test
    void updateListRemovesNilTerminalFromPreviouslyLastNodeAndAppendsItToNewLastNode() throws Exception {
        final ReferencedListValueDescriptor<NamedResource> descriptor = createValueDescriptor(Assertion.AssertionType.OBJECT_PROPERTY, true);
        final List<NamedResource> addedItems = IntStream.range(0, 5)
                                                        .mapToObj(i -> NamedResource.create(Vocabulary.INDIVIDUAL_IRI_BASE + i))
                                                        .toList();
        final List<NamedResource> oldList = generateList();
        final List<URI> oldNodes = initListNodes(oldList);
        initStatementsForList(oldNodes, oldList);
        final IRI lastNodeIri = vf.createIRI(oldNodes.get(oldNodes.size() - 1).toString());
        final Statement nilTerminal = vf.createStatement(lastNodeIri, vf.createIRI(NEXT_NODE_PROPERTY), RDF.NIL);
        when(connector.findStatements(lastNodeIri, vf.createIRI(NEXT_NODE_PROPERTY), null, false, Collections.emptySet()))
                .thenReturn(Set.of(nilTerminal));
        // The original items
        for (NamedResource item : oldList) {
            descriptor.addValue(item);
        }
        // Now add the new ones
        for (NamedResource r : addedItems) {
            descriptor.addValue(r);
        }

        sut.updateList(descriptor);
        verify(connector).removeStatements(Set.of(nilTerminal));
        verify(connector, atLeast(1)).addStatements(anyCollection());
        // Added nodes with values and terminal
        assertEquals(addedItems.size() * 2 + 1, added.size());
        for (Statement stmt : added) {
            if (stmt.getPredicate().equals(nodeContentProperty)) {
                final URI u = URI.create(stmt.getObject().stringValue());
                assertTrue(addedItems.contains(NamedResource.create(u)));
            }
        }
        assertTrue(added.stream().anyMatch(s -> s.getObject().equals(RDF.NIL)));
    }

    @Test
    void updateListRemovesNilTerminalFromLastNodeWhenItIsRemovedAndAddsItToNewLastNode() throws Exception {
        final ReferencedListValueDescriptor<NamedResource> descriptor = createValueDescriptor(Assertion.AssertionType.OBJECT_PROPERTY, true);
        final List<NamedResource> oldList = generateList();
        final List<URI> oldNodes = initListNodes(oldList);
        initStatementsForList(oldNodes, oldList);
        final IRI lastNodeIri = vf.createIRI(oldNodes.get(oldNodes.size() - 1).toString());
        final Statement nilTerminal = vf.createStatement(lastNodeIri, vf.createIRI(NEXT_NODE_PROPERTY), RDF.NIL);
        when(connector.findStatements(lastNodeIri, vf.createIRI(NEXT_NODE_PROPERTY), null, false, Collections.emptySet()))
                .thenReturn(Set.of(nilTerminal));
        // The original items
        for (NamedResource item : oldList.subList(0, oldList.size() / 2)) {
            descriptor.addValue(item);
        }
        sut.updateList(descriptor);
        assertThat(removed, hasItem(nilTerminal));
        assertTrue(added.stream().anyMatch(s -> s.getObject().equals(RDF.NIL)));
    }
}

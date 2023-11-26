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
package cz.cvut.kbss.ontodriver.rdf4j;

import cz.cvut.kbss.ontodriver.descriptor.ReferencedListDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.ReferencedListDescriptorImpl;
import cz.cvut.kbss.ontodriver.descriptor.ReferencedListValueDescriptor;
import cz.cvut.kbss.ontodriver.exception.IntegrityConstraintViolatedException;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.rdf4j.connector.Connector;
import cz.cvut.kbss.ontodriver.rdf4j.environment.Generator;
import cz.cvut.kbss.ontodriver.rdf4j.util.Rdf4jUtils;
import org.eclipse.rdf4j.model.IRI;
import org.eclipse.rdf4j.model.Literal;
import org.eclipse.rdf4j.model.Resource;
import org.eclipse.rdf4j.model.Statement;
import org.eclipse.rdf4j.model.Value;
import org.eclipse.rdf4j.model.ValueFactory;
import org.eclipse.rdf4j.model.impl.SimpleValueFactory;
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
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Random;
import java.util.stream.IntStream;

import static cz.cvut.kbss.ontodriver.rdf4j.ListHandlerTestHelper.LIST_PROPERTY;
import static cz.cvut.kbss.ontodriver.rdf4j.ListHandlerTestHelper.NEXT_NODE_PROPERTY;
import static cz.cvut.kbss.ontodriver.rdf4j.ListHandlerTestHelper.OWNER;
import static cz.cvut.kbss.ontodriver.rdf4j.ListHandlerTestHelper.generateList;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.anyBoolean;
import static org.mockito.Mockito.anyCollection;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@SuppressWarnings({"unchecked", "rawtypes"})
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class ReferencedListHandlerTest {

    private static final ValueFactory vf = SimpleValueFactory.getInstance();
    private static final Resource owner = vf.createIRI(OWNER.toString());

    private static final IRI hasListProperty = vf.createIRI(LIST_PROPERTY);
    private static final IRI nextNodeProperty = vf.createIRI(NEXT_NODE_PROPERTY);
    private static final IRI nodeContentProperty = vf.createIRI(ListHandlerTestHelper.NODE_CONTENT_PROPERTY);

    private static final Assertion hasListAssertion = Assertion.createObjectPropertyAssertion(URI.create(LIST_PROPERTY), false);
    private static final Assertion nextNodeAssertion = Assertion.createObjectPropertyAssertion(URI.create(NEXT_NODE_PROPERTY), false);
    private static final Assertion nodeContentAssertion = Assertion.createObjectPropertyAssertion(
            URI.create(ListHandlerTestHelper.NODE_CONTENT_PROPERTY), false);

    @Mock
    private Connector connector;

    private ReferencedListDescriptor listDescriptor;

    private ReferencedListHandler handler;

    private final List<Statement> added = new ArrayList<>();
    private final List<Statement> removed = new ArrayList<>();

    @BeforeEach
    public void setUp() throws Exception {
        this.listDescriptor = new ReferencedListDescriptorImpl(OWNER, hasListAssertion, nextNodeAssertion, nodeContentAssertion);

        this.handler = new ReferencedListHandler(connector, vf);
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
        final Collection<Axiom<NamedResource>> res = handler.loadList(listDescriptor);
        assertEquals(refList.size(), res.size());
        for (Axiom<NamedResource> a : res) {
            assertTrue(refList.contains(a.getValue().getValue()));
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
                                                  List<NamedResource> values) throws Exception {
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
            final Statement content = vf.createStatement(itemUri, nodeContentProperty,
                    vf.createIRI(values.get(i).toString()));
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
        assertThrows(IntegrityConstraintViolatedException.class, () -> handler.loadList(listDescriptor));
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
        assertThrows(IntegrityConstraintViolatedException.class, () -> handler.loadList(listDescriptor));
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
        final List<Statement> stmts = Arrays.asList(mock(Statement.class), mock(Statement.class));
        when(connector.findStatements(eq(node), eq(property), eq(null),
                anyBoolean(), eq(Collections.emptySet()))).thenReturn(stmts);
        assertThrows(IntegrityConstraintViolatedException.class, () -> handler.loadList(listDescriptor));
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
        handler.persistList(valueDescriptor);
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
        handler.persistList(spiedValues);
        verify(spiedValues).getValues();
        verify(connector, never()).addStatements(anyCollection());
    }

    @Test
    public void clearsListOnUpdateWhenDescriptorHasNoValues() throws Exception {
        final ReferencedListValueDescriptor<NamedResource> descriptor = initValues(0);
        // old list
        final List<NamedResource> refList = generateList();
        final List<Statement> oldList = initStatementsForList(initListNodes(refList), refList);

        handler.updateList(descriptor);
        verify(connector).removeStatements(anyCollection());
        verify(connector, never()).addStatements(anyCollection());
        assertEquals(oldList.size(), removed.size());
        assertTrue(removed.containsAll(oldList));
    }

    private static ReferencedListValueDescriptor<NamedResource> initValues(int count) {
        final ReferencedListValueDescriptor<NamedResource> desc = new ReferencedListValueDescriptor<>(OWNER,
                Assertion.createObjectPropertyAssertion(URI.create(LIST_PROPERTY), false),
                Assertion.createObjectPropertyAssertion(URI.create(NEXT_NODE_PROPERTY),
                        false), Assertion.createObjectPropertyAssertion(
                URI.create(ListHandlerTestHelper.NODE_CONTENT_PROPERTY), false));
        for (int i = 0; i < count; i++) {
            desc.addValue(NamedResource.create("http://krizik.felk.cvut.cz/ontologies/jopa/entityA_" + i));
        }
        return desc;
    }

    @Test
    public void insertsListOnUpdateWhenThereWereNoValuesBefore() throws Exception {
        final ReferencedListValueDescriptor<NamedResource> descriptor = initValues(5);
        when(
                connector.findStatements(owner, hasListProperty, null, descriptor.getListProperty().isInferred()))
                .thenReturn(Collections.emptyList());

        handler.updateList(descriptor);
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

        handler.updateList(descriptor);
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
        final ReferencedListValueDescriptor<Integer> desc = new ReferencedListValueDescriptor<>(OWNER,
                Assertion.createObjectPropertyAssertion(URI.create(LIST_PROPERTY), false),
                Assertion.createObjectPropertyAssertion(URI.create(NEXT_NODE_PROPERTY),
                        false), Assertion.createDataPropertyAssertion(URI.create(ListHandlerTestHelper.NODE_CONTENT_PROPERTY), false));
        IntStream.range(0, 5).mapToObj(i -> Generator.randomInt()).forEach(desc::addValue);

        final ReferencedListHandler sut = new ReferencedListHandler(connector, vf);
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
}

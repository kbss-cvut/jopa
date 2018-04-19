/**
 * Copyright (C) 2016 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.ontodriver.sesame;

import cz.cvut.kbss.ontodriver.descriptor.ListValueDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.ReferencedListDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.ReferencedListDescriptorImpl;
import cz.cvut.kbss.ontodriver.descriptor.ReferencedListValueDescriptor;
import cz.cvut.kbss.ontodriver.exception.IntegrityConstraintViolatedException;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import org.eclipse.rdf4j.model.IRI;
import org.eclipse.rdf4j.model.Resource;
import org.eclipse.rdf4j.model.Statement;
import org.eclipse.rdf4j.model.Value;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.MockitoAnnotations;

import java.util.*;

import static org.junit.Assert.*;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyBoolean;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.*;

@SuppressWarnings({"unchecked", "rawtypes"})
public class ReferencedListHandlerTest extends ListHandlerTestBase<ReferencedListDescriptor, ReferencedListValueDescriptor> {

    private static final String NODE_CONTENT_PROPERTY =
            "http://krizik.felk.cvut.cz/ontologies/2008/6/sequences.owl#hasContents";

    private static IRI nodeContentProperty;

    private List<Statement> added = new ArrayList<>();
    private List<Statement> removed = new ArrayList<>();

    @BeforeClass
    public static void setUpBeforeClass() {
        init();
        nodeContentProperty = vf.createIRI(NODE_CONTENT_PROPERTY);
    }

    @Before
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        final Assertion listProperty = Assertion.createObjectPropertyAssertion(
                java.net.URI.create(LIST_PROPERTY), false);
        final Assertion nextNodeProperty = Assertion.createObjectPropertyAssertion(
                java.net.URI.create(NEXT_NODE_PROPERTY), false);
        final Assertion nodeContentProperty = Assertion.createObjectPropertyAssertion(
                java.net.URI.create(NODE_CONTENT_PROPERTY), false);
        this.listDescriptor = new ReferencedListDescriptorImpl(OWNER, listProperty,
                nextNodeProperty, nodeContentProperty);
        this.valueDescriptor = new ReferencedListValueDescriptor(OWNER, listProperty,
                nextNodeProperty, nodeContentProperty);

        this.handler = new ReferencedListHandler(connector, vf);
        doAnswer(invocation -> {
            final Collection<Statement> arg = (Collection<Statement>) invocation.getArguments()[0];
            added.addAll(arg);
            return null;
        }).when(connector).addStatements(any(Collection.class));
        doAnswer(invocation -> {
            final Collection<Statement> arg = (Collection<Statement>) invocation.getArguments()[0];
            removed.addAll(arg);
            return null;
        }).when(connector).removeStatements(any(Collection.class));
    }

    @Test
    public void staticFactoryMethodForReferencedLists() {
        final ListHandler<?, ?> h = ListHandler.createForReferencedList(connector, vf);
        assertNotNull(h);
        assertTrue(h instanceof ReferencedListHandler);
    }

    @Test
    public void loadsReferencedList() throws Exception {
        final List<NamedResource> refList = initList();
        final List<java.net.URI> listNodes = initListNodes(refList);
        initStatementsForList(listNodes, refList);
        final Collection<Axiom<NamedResource>> res = handler.loadList(listDescriptor);
        assertEquals(refList.size(), res.size());
        for (Axiom<NamedResource> a : res) {
            assertTrue(refList.contains(a.getValue().getValue()));
        }
    }

    private List<java.net.URI> initListNodes(List<?> content) {
        final List<java.net.URI> nodes = new ArrayList<>();
        for (int i = 0; i < content.size(); i++) {
            nodes.add(java.net.URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/SEQ_" + i));
        }
        return nodes;
    }

    private List<Statement> initStatementsForList(List<java.net.URI> nodes,
                                                  List<NamedResource> values) throws Exception {
        int i = 0;
        Resource prev = owner;
        final List<Statement> stmts = new ArrayList<>();
        for (java.net.URI item : nodes) {
            final IRI itemUri = vf.createIRI(item.toString());
            Statement node;
            if (i == 0) {
                node = vf.createStatement(prev, hasListProperty, itemUri);
                when(
                        connector.findStatements(eq(prev), eq(hasListProperty), eq(null),
                                anyBoolean(), eq(null))).thenReturn(
                        Collections.singleton(node));
            } else {
                node = vf.createStatement(prev, nextNodeProperty, itemUri);
                when(
                        connector.findStatements(eq(prev), eq(nextNodeProperty), eq(null),
                                anyBoolean(), eq(null))).thenReturn(
                        Collections.singleton(node));
            }
            stmts.add(node);
            final Statement content = vf.createStatement(itemUri, nodeContentProperty,
                    vf.createIRI(values.get(i).toString()));
            when(
                    connector.findStatements(eq(itemUri), eq(nodeContentProperty),
                            eq(null), anyBoolean(), eq(null))).thenReturn(
                    Collections.singleton(content));
            stmts.add(content);
            prev = itemUri;
            i++;
        }
        return stmts;
    }

    @Test(expected = IntegrityConstraintViolatedException.class)
    public void throwsICViolationWhenThereIsNoContentInHeadNode() throws Exception {
        final IRI headNode = vf.createIRI("http://krizik.felk.cvut.cz/ontologies/jopa/SEQ0");
        when(
                connector.findStatements(eq(owner), eq(hasListProperty), eq(null),
                        anyBoolean(), eq(null))).thenReturn(
                Collections.singleton(vf.createStatement(owner, hasListProperty, headNode)));
        when(
                connector.findStatements(eq(headNode), eq(nodeContentProperty), eq(null),
                        anyBoolean(), eq(null))).thenReturn(Collections.emptyList());
        try {
            final Collection<Axiom<NamedResource>> res = handler.loadList(listDescriptor);
            assert res == null;
            fail("This line should not have been reached.");
        } finally {
            verify(connector, never()).findStatements(any(Resource.class), eq(nextNodeProperty),
                    any(Value.class), anyBoolean());
        }
    }

    @Test(expected = IntegrityConstraintViolatedException.class)
    public void throwsICViolationWhenThereIsNoContentInSomeListNode() throws Exception {
        final List<NamedResource> refList = initList();
        final List<java.net.URI> listNodes = initListNodes(refList);
        initStatementsForList(listNodes, refList);
        final Resource elem = selectRandomNode(listNodes);
        when(
                connector.findStatements(eq(elem), eq(nodeContentProperty), eq(null),
                        anyBoolean(), eq(null))).thenReturn(Collections.emptyList());
        final Collection<Axiom<NamedResource>> res = handler.loadList(listDescriptor);
        assert res == null;
    }

    private Resource selectRandomNode(List<java.net.URI> nodes) {
        // Select a random index, but it shouldn't be 0 (it would be the head),
        // so add 1
        final int rand = new Random().nextInt(nodes.size() - 1) + 1;
        return vf.createIRI(nodes.get(rand).toString());
    }

    @Test(expected = IntegrityConstraintViolatedException.class)
    public void throwsICViolationWhenThereAreMultipleSuccessorsForNode() throws Exception {
        runIcViolationTest(nextNodeProperty);
    }

    private void runIcViolationTest(IRI property) throws Exception {
        final List<NamedResource> refList = initList();
        final List<java.net.URI> listNodes = initListNodes(refList);
        initStatementsForList(listNodes, refList);
        final Resource node = selectRandomNode(listNodes);
        final List<Statement> stmts = Arrays.asList(mock(Statement.class), mock(Statement.class));
        when(
                connector.findStatements(eq(node), eq(property), eq(null),
                        anyBoolean(), eq(null))).thenReturn(stmts);
        handler.loadList(listDescriptor);
    }

    @Test(expected = IntegrityConstraintViolatedException.class)
    public void throwsICViolationWhenThereAreMultipleReferencesInNode() throws Exception {
        runIcViolationTest(nodeContentProperty);
    }

    @Test
    public void persistsReferencedList() throws Exception {
        final List<NamedResource> values = initList();
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
        assertTrue(valueDescriptor.getValues().isEmpty());
        final ReferencedListValueDescriptor spiedValues = spy(valueDescriptor);
        handler.persistList(spiedValues);
        verify(spiedValues).getValues();
        verify(connector, never()).addStatements(any(Collection.class));
    }

    @Test
    public void clearsListOnUpdateWhenDescriptorHasNoValues() throws Exception {
        final ReferencedListValueDescriptor descriptor = initValues(0);
        // old list
        final List<NamedResource> refList = initList();
        final List<Statement> oldList = initStatementsForList(initListNodes(refList), refList);

        handler.updateList(descriptor);
        verify(connector).removeStatements(any(Collection.class));
        verify(connector, never()).addStatements(any(Collection.class));
        assertEquals(oldList.size(), removed.size());
        assertTrue(removed.containsAll(oldList));
    }

    private static ReferencedListValueDescriptor initValues(int count) {
        final ReferencedListValueDescriptor desc = new ReferencedListValueDescriptor(OWNER,
                Assertion.createObjectPropertyAssertion(java.net.URI.create(LIST_PROPERTY), false),
                Assertion.createObjectPropertyAssertion(java.net.URI.create(NEXT_NODE_PROPERTY),
                        false), Assertion.createObjectPropertyAssertion(
                java.net.URI.create(NODE_CONTENT_PROPERTY), false));
        for (int i = 0; i < count; i++) {
            desc.addValue(NamedResource
                    .create("http://krizik.felk.cvut.cz/ontologies/jopa/entityA_" + i));
        }
        return desc;
    }

    @Test
    public void insertsListOnUpdateWhenThereWereNoValuesBefore() throws Exception {
        final ReferencedListValueDescriptor descriptor = initValues(5);
        when(
                connector.findStatements(owner, hasListProperty, null, descriptor.getListProperty().isInferred()))
                .thenReturn(Collections.emptyList());

        handler.updateList(descriptor);
        verify(connector, never()).removeStatements(any(Collection.class));
        int i = 0;
        assertFalse(added.isEmpty());
        for (Statement stmt : added) {
            if (stmt.getPredicate().equals(nodeContentProperty)) {
                assertEquals(descriptor.getValues().get(i++).getIdentifier(),
                        java.net.URI.create(stmt.getObject().stringValue()));
            }
        }
    }

    @Test
    public void updateListAddsNewValuesToTheEnd() throws Exception {
        final ReferencedListValueDescriptor descriptor = initValues(0);
        final ReferencedListValueDescriptor tempDesc = initValues(8);
        final List<NamedResource> refList = initList();
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
        verify(connector, never()).removeStatements(any(Collection.class));
        verify(connector, atLeast(1)).addStatements(any(Collection.class));
        assertEquals(tempDesc.getValues().size() * 2, added.size());
        for (Statement stmt : added) {
            if (stmt.getPredicate().equals(nodeContentProperty)) {
                final java.net.URI u = java.net.URI.create(stmt.getObject().stringValue());
                assertTrue(tempDesc.getValues().contains(NamedResource.create(u)));
            }
        }
    }
}

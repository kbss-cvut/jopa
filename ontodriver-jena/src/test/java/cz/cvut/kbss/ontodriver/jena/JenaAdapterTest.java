/*
 * JOPA
 * Copyright (C) 2026 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.jena;

import cz.cvut.kbss.ontodriver.Statement.StatementOntology;
import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.AxiomValueDescriptor;
import cz.cvut.kbss.ontodriver.jena.connector.InferredStorageConnector;
import cz.cvut.kbss.ontodriver.jena.connector.StorageConnector;
import cz.cvut.kbss.ontodriver.jena.connector.SubjectPredicateContext;
import cz.cvut.kbss.ontodriver.jena.environment.Generator;
import cz.cvut.kbss.ontodriver.jena.query.AbstractResultSet;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.AxiomImpl;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;
import org.apache.jena.query.Query;
import org.apache.jena.query.QueryFactory;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.ResourceFactory;
import org.apache.jena.rdf.model.Statement;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InOrder;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

import java.net.URI;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.anyList;
import static org.mockito.Mockito.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class JenaAdapterTest {

    private static final NamedResource SUBJECT = NamedResource.create(Generator.generateUri());
    private static final Resource SUBJECT_RESOURCE = ResourceFactory.createResource(SUBJECT.getIdentifier().toString());

    @Mock
    private StorageConnector connectorMock;

    @Mock
    private InferredStorageConnector inferredConnectorMock;

    private JenaAdapter adapter;

    @BeforeEach
    void setUp() {
        this.adapter = new JenaAdapter(connectorMock, inferredConnectorMock);
    }

    @Test
    void persistBeginsTransaction() {
        final AxiomValueDescriptor descriptor = new AxiomValueDescriptor(SUBJECT);
        adapter.persist(descriptor);
        verify(connectorMock).begin();
    }

    @Test
    void persistGeneratesStatementsAndPassesThemToConnectorForPersist() {
        final AxiomValueDescriptor descriptor = new AxiomValueDescriptor(SUBJECT);
        final Assertion a = Assertion.createClassAssertion(false);
        final NamedResource type = NamedResource.create(Generator.generateUri());
        descriptor.addAssertionValue(a, new Value<>(type));
        adapter.persist(descriptor);
        final ArgumentCaptor<List<Statement>> captor = ArgumentCaptor.forClass(List.class);
        verify(connectorMock).add(captor.capture(), eq(null));
        final List<Statement> arg = captor.getValue();
        assertEquals(1, arg.size());
    }

    @Test
    void commitDataToStorage() throws Exception {
        final AxiomValueDescriptor descriptor = new AxiomValueDescriptor(SUBJECT);
        final Assertion a = Assertion.createClassAssertion(false);
        final NamedResource type = NamedResource.create(Generator.generateUri());
        descriptor.addAssertionValue(a, new Value<>(type));
        adapter.persist(descriptor);
        adapter.commit();
        verify(connectorMock).add(anyList(), eq(null));
        verify(connectorMock).commit();
    }

    @Test
    void commitDoesNothingWhenTransactionIsNotActive() throws Exception {
        adapter.commit();
        verify(connectorMock, never()).commit();
    }

    @Test
    void rollbackRollsBackChanges() throws Exception {
        final AxiomValueDescriptor descriptor = new AxiomValueDescriptor(SUBJECT);
        final Assertion a = Assertion.createClassAssertion(false);
        final NamedResource type = NamedResource.create(Generator.generateUri());
        descriptor.addAssertionValue(a, new Value<>(type));
        adapter.persist(descriptor);
        adapter.rollback();
        verify(connectorMock).add(anyList(), eq(null));
        verify(connectorMock).rollback();
        verify(connectorMock, never()).commit();
    }

    @Test
    void rollbackDoesNothingWhenTransactionIsNotActive() {
        adapter.rollback();
        verify(connectorMock, never()).rollback();
    }

    @Test
    void closeClosesConnector() throws Exception {
        adapter.close();
        verify(connectorMock).close();
    }

    @Test
    void containsStartsTransactionWhenItIsNotActive() {
        final String typeUri = Generator.generateUri().toString();
        final Axiom<?> ax = new AxiomImpl<>(SUBJECT, Assertion.createClassAssertion(false),
                new Value<>(NamedResource.create(typeUri)));
        adapter.contains(ax, Collections.singleton(Generator.generateUri()));
        verify(connectorMock).begin();
    }

    @Test
    void containsChecksForStatementExistence() {
        final String typeUri = Generator.generateUri().toString();
        final Axiom<?> ax = new AxiomImpl<>(SUBJECT, Assertion.createClassAssertion(false),
                new Value<>(NamedResource.create(typeUri)));
        when(connectorMock.contains(any(), any(), any(), any())).thenReturn(true);
        assertTrue(adapter.contains(ax, Collections.emptySet()));
    }

    @Test
    void getContextsListsContextsFromConnectorAndTransformsThemToUris() {
        final List<String> contexts = IntStream.range(0, 5).mapToObj(i -> Generator.generateUri().toString()).collect(
                Collectors.toList());
        when(connectorMock.getContexts()).thenReturn(contexts);
        final List<URI> result = adapter.getContext();
        assertEquals(contexts.size(), result.size());
        result.forEach(u -> assertTrue(contexts.contains(u.toString())));
    }

    @Test
    void unwrapReturnsAdapterInstanceWhenTargetClassMatches() throws Exception {
        final JenaAdapter result = adapter.unwrap(JenaAdapter.class);
        assertSame(adapter, result);
    }

    @Test
    void unwrapPassesCallToStorageConnectorWhenTargetClassDoesNotMatch() throws Exception {
        adapter.unwrap(StorageConnector.class);
        verify(connectorMock).unwrap(StorageConnector.class);
    }

    @Test
    void findLoadsAxiomsFromStorage() {
        final AxiomDescriptor descriptor = new AxiomDescriptor(SUBJECT);
        final Assertion assertion = Assertion.createObjectPropertyAssertion(Generator.generateUri(), false);
        descriptor.addAssertion(assertion);
        final Statement s = ResourceFactory
                .createStatement(SUBJECT_RESOURCE, assertionToProperty(assertion),
                        ResourceFactory.createResource(Generator.generateUri().toString()));
        when(connectorMock.find(any(), any(), any(), any())).thenReturn(Collections.singletonList(s));

        final Collection<Axiom<?>> result = adapter.find(descriptor);
        assertEquals(1, result.size());
        final Axiom<?> axiom = result.iterator().next();
        assertEquals(SUBJECT, axiom.getSubject());
        assertEquals(assertion, axiom.getAssertion());
        assertEquals(s.getObject().asResource().getURI(), axiom.getValue().stringValue());
        verify(connectorMock).find(SUBJECT_RESOURCE, null, null, Collections.emptySet());
    }

    private static Property assertionToProperty(Assertion assertion) {
        return ResourceFactory.createProperty(assertion.getIdentifier().toString());
    }

    @Test
    void removeRemovesStatementsFromStorage() {
        final AxiomDescriptor descriptor = new AxiomDescriptor(SUBJECT);
        final Assertion assertion = Assertion.createObjectPropertyAssertion(Generator.generateUri(), false);
        descriptor.addAssertion(assertion);

        adapter.remove(descriptor);
        verify(connectorMock).removePropertyValues(Set.of(new SubjectPredicateContext(SUBJECT_RESOURCE, assertionToProperty(assertion), Collections.emptySet())));
    }

    @Test
    void updateRemovesOldStatementsAndInsertsNewOnes() {
        final Assertion assertion = Assertion.createObjectPropertyAssertion(Generator.generateUri(), false);
        final URI newValue = Generator.generateUri();
        final AxiomValueDescriptor descriptor = new AxiomValueDescriptor(SUBJECT);
        descriptor.addAssertionValue(assertion, new Value<>(NamedResource.create(newValue)));

        adapter.update(descriptor);
        verify(connectorMock).removePropertyValues(Set.of(new SubjectPredicateContext(SUBJECT_RESOURCE, assertionToProperty(assertion), Collections.emptySet())));
        final ArgumentCaptor<List<Statement>> captor = ArgumentCaptor.forClass(List.class);
        verify(connectorMock).add(captor.capture(), eq(null));
        assertEquals(1, captor.getValue().size());
        final Statement result = captor.getValue().get(0);
        assertEquals(SUBJECT_RESOURCE, result.getSubject());
        assertEquals(assertionToProperty(assertion), result.getPredicate());
        assertEquals(ResourceFactory.createResource(newValue.toString()), result.getObject());
    }

    @Test
    void isConsistentChecksForConsistencyOnInferredConnector() {
        adapter.isConsistent(null);
        verify(inferredConnectorMock).isConsistent(null);
    }

    @Test
    void isConsistentStartsTransactionIfNotAlreadyActive() {
        final URI context = Generator.generateUri();
        adapter.isConsistent(context);
        verify(connectorMock).begin();
        verify(inferredConnectorMock).isConsistent(context.toString());
    }

    @Test
    void executeSelectQueryStartsTransactionIfNotAlreadyActiveAndExecutesQuery() throws Exception {
        final Query query = QueryFactory.create("SELECT * WHERE { ?x ?y ?z . }");
        final AbstractResultSet rs = mock(AbstractResultSet.class);
        when(inferredConnectorMock.executeSelectQuery(any(), any())).thenReturn(rs);
        final AbstractResultSet result = adapter.executeSelectQuery(query, StatementOntology.SHARED);
        assertEquals(rs, result);
        final InOrder inOrder = Mockito.inOrder(inferredConnectorMock, connectorMock);
        inOrder.verify(connectorMock).begin();
        inOrder.verify(inferredConnectorMock).executeSelectQuery(query, StatementOntology.SHARED);
    }

    @Test
    void executeAskQueryStartsTransactionIfNotAlreadyActiveAndExecutesQuery() throws Exception {
        final Query query = QueryFactory.create("ASK WHERE { ?x ?y ?z . }");
        final AbstractResultSet rs = mock(AbstractResultSet.class);
        when(inferredConnectorMock.executeAskQuery(any(), any())).thenReturn(rs);
        final AbstractResultSet result = adapter.executeAskQuery(query, StatementOntology.TRANSACTIONAL);
        assertEquals(rs, result);
        final InOrder inOrder = Mockito.inOrder(inferredConnectorMock, connectorMock);
        inOrder.verify(connectorMock).begin();
        inOrder.verify(inferredConnectorMock).executeAskQuery(query, StatementOntology.TRANSACTIONAL);
    }

    @Test
    void executeUpdateQueryStartsTransactionIfNotAlreadyActiveAndExecutesQuery() throws Exception {
        adapter.executeUpdate("INSERT DATA { ex:a a ex:b }", StatementOntology.TRANSACTIONAL);
        final InOrder inOrder = Mockito.inOrder(inferredConnectorMock, connectorMock);
        inOrder.verify(connectorMock).begin();
        inOrder.verify(inferredConnectorMock)
               .executeUpdate("INSERT DATA { ex:a a ex:b }", StatementOntology.TRANSACTIONAL);
    }
}

/**
 * Copyright (C) 2019 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License along with this program. If not, see
 * <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.ontodriver.jena;

import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.AxiomValueDescriptor;
import cz.cvut.kbss.ontodriver.jena.connector.InferredStorageConnector;
import cz.cvut.kbss.ontodriver.jena.connector.StorageConnector;
import cz.cvut.kbss.ontodriver.jena.environment.Generator;
import cz.cvut.kbss.ontodriver.jena.query.JenaPreparedStatement;
import cz.cvut.kbss.ontodriver.jena.query.JenaStatement;
import cz.cvut.kbss.ontodriver.model.*;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.ResourceFactory;
import org.apache.jena.rdf.model.Statement;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

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
        MockitoAnnotations.initMocks(this);
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
        final ArgumentCaptor<List> captor = ArgumentCaptor.forClass(List.class);
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
        adapter.contains(ax, Generator.generateUri());
        verify(connectorMock).begin();
    }

    @Test
    void containsChecksForStatementExistence() {
        final String typeUri = Generator.generateUri().toString();
        final Axiom<?> ax = new AxiomImpl<>(SUBJECT, Assertion.createClassAssertion(false),
                new Value<>(NamedResource.create(typeUri)));
        when(connectorMock.contains(any(), any(), any(), any())).thenReturn(true);
        assertTrue(adapter.contains(ax, null));
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
        verify(connectorMock).find(SUBJECT_RESOURCE, null, null, null);
    }

    private Property assertionToProperty(Assertion assertion) {
        return ResourceFactory.createProperty(assertion.getIdentifier().toString());
    }

    @Test
    void removeRemovesStatementsFromStorage() {
        final AxiomDescriptor descriptor = new AxiomDescriptor(SUBJECT);
        final Assertion assertion = Assertion.createObjectPropertyAssertion(Generator.generateUri(), false);
        descriptor.addAssertion(assertion);

        adapter.remove(descriptor);
        verify(connectorMock).remove(SUBJECT_RESOURCE, assertionToProperty(assertion), null, null);
    }

    @Test
    void updateRemovesOldStatementsAndInsertsNewOnes() {
        final Assertion assertion = Assertion.createObjectPropertyAssertion(Generator.generateUri(), false);
        final Statement s = ResourceFactory
                .createStatement(SUBJECT_RESOURCE, assertionToProperty(assertion),
                        ResourceFactory.createResource(Generator.generateUri().toString()));
        when(connectorMock.find(any(), any(), any(), anyString())).thenReturn(Collections.singletonList(s));
        final URI newValue = Generator.generateUri();
        final AxiomValueDescriptor descriptor = new AxiomValueDescriptor(SUBJECT);
        descriptor.addAssertionValue(assertion, new Value<>(NamedResource.create(newValue)));

        adapter.update(descriptor);
        verify(connectorMock).remove(SUBJECT_RESOURCE, assertionToProperty(assertion), null, null);
        final ArgumentCaptor<List> captor = ArgumentCaptor.forClass(List.class);
        verify(connectorMock).add(captor.capture(), eq(null));
        assertEquals(1, captor.getValue().size());
        final Statement result = (Statement) captor.getValue().get(0);
        assertEquals(SUBJECT_RESOURCE, result.getSubject());
        assertEquals(assertionToProperty(assertion), result.getPredicate());
        assertEquals(ResourceFactory.createResource(newValue.toString()), result.getObject());
    }

    @Test
    void createStatementReturnsNewJenaStatement() throws Exception {
        final JenaStatement result = adapter.createStatement();
        assertNotNull(result);
        final Field execField = JenaStatement.class.getDeclaredField("executor");
        execField.setAccessible(true);
        assertSame(inferredConnectorMock, execField.get(result));
    }

    @Test
    void createStatementStartsTransactionIfNotAlreadyActive() {
        adapter.createStatement();
        verify(connectorMock).begin();
    }

    @Test
    void prepareStatementReturnsNewPreparedStatement() throws Exception {
        final JenaPreparedStatement result = adapter.prepareStatement("SELECT * WHERE {?x ?y ?z . }");
        assertNotNull(result);
        final Field execField = JenaStatement.class.getDeclaredField("executor");
        execField.setAccessible(true);
        assertSame(inferredConnectorMock, execField.get(result));
    }

    @Test
    void prepareStatementStartsTransactionIfNotAlreadyActive() {
        adapter.prepareStatement("SELECT * WHERE {?x ?y ?z . }");
        verify(connectorMock).begin();
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
}
/**
 * Copyright (C) 2020 Czech Technical University in Prague
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
import cz.cvut.kbss.ontodriver.jena.connector.StorageConnector;
import cz.cvut.kbss.ontodriver.jena.environment.Generator;
import cz.cvut.kbss.ontodriver.jena.list.JenaLists;
import cz.cvut.kbss.ontodriver.jena.util.ConnectionListener;
import cz.cvut.kbss.ontodriver.model.*;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InOrder;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import java.net.URI;
import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

public class JenaConnectionTest {

    private static final NamedResource SUBJECT = NamedResource.create(Generator.generateUri());

    @Mock
    private JenaAdapter adapterMock;

    private JenaConnection connection;

    @BeforeEach
    public void setUp() {
        MockitoAnnotations.initMocks(this);
        this.connection = new JenaConnection(adapterMock);
    }

    @Test
    public void setAutoCommitThrowsIllegalStateForClosedConnection() throws Exception {
        connection.close();
        assertThrows(IllegalStateException.class, () -> connection.setAutoCommit(false));
    }

    @Test
    public void isAutoCommitThrowsIllegalStateForClosedConnection() throws Exception {
        connection.close();
        assertThrows(IllegalStateException.class, () -> connection.isAutoCommit());
    }

    @Test
    public void closeClosesUnderlyingAdapter() throws Exception {
        connection.close();
        assertFalse(connection.isOpen());
        verify(adapterMock).close();
    }

    @Test
    public void commitCommitsUnderlyingAdapter() throws Exception {
        connection.commit();
        verify(adapterMock).commit();
    }

    @Test
    public void commitDoesNothingForAutoCommitConnection() throws Exception {
        connection.setAutoCommit(true);
        connection.commit();
        verify(adapterMock, never()).commit();
    }

    @Test
    public void commitThrowsIllegalStateExceptionForClosedConnection() throws Exception {
        connection.close();
        assertThrows(IllegalStateException.class, () -> connection.commit());
    }

    @Test
    public void rollbackRollsBackChangesInAdapter() {
        connection.rollback();
        verify(adapterMock).rollback();
    }

    @Test
    public void rollbackDoesNothingForAutoCommitConnection() {
        connection.setAutoCommit(true);
        connection.rollback();
        verify(adapterMock, never()).rollback();
    }

    @Test
    public void rollbackThrowsIllegalStateExceptionForClosedConnection() throws Exception {
        connection.close();
        assertThrows(IllegalStateException.class, () -> connection.rollback());
    }

    @Test
    public void persistPassesValueDescriptorToUnderlyingAdapter() throws Exception {
        final AxiomValueDescriptor descriptor = new AxiomValueDescriptor(SUBJECT);
        connection.persist(descriptor);
        verify(adapterMock).persist(descriptor);
    }

    @Test
    public void persistCommitsTransactionIfConnectionIsAutoCommit() throws Exception {
        final AxiomValueDescriptor descriptor = new AxiomValueDescriptor(SUBJECT);
        connection.setAutoCommit(true);
        connection.persist(descriptor);
        final InOrder inOrder = Mockito.inOrder(adapterMock);
        inOrder.verify(adapterMock).persist(descriptor);
        inOrder.verify(adapterMock).commit();
    }

    @Test
    public void persistThrowsIllegalStateExceptionForClosedConnection() throws Exception {
        final AxiomValueDescriptor descriptor = new AxiomValueDescriptor(SUBJECT);
        connection.close();
        assertThrows(IllegalStateException.class, () -> connection.persist(descriptor));
    }

    @Test
    public void closeNotifiesRegisteredListeners() throws Exception {
        final ConnectionListener listener = mock(ConnectionListener.class);
        connection.registerListener(listener);
        connection.close();
        verify(listener).connectionClosed(connection);
    }

    @Test
    public void unwrapReturnsConnectionInstanceIfClassMatches() throws Exception {
        final JenaConnection result = connection.unwrap(JenaConnection.class);
        assertSame(connection, result);
    }

    @Test
    public void unwrapPassesCallToAdapterWhenClassDoesNotMatch() throws Exception {
        connection.unwrap(StorageConnector.class);
        verify(adapterMock).unwrap(StorageConnector.class);
    }

    @Test
    public void unwrapThrowsIllegalStateExceptionForClosedConnection() throws Exception {
        connection.close();
        assertThrows(IllegalStateException.class, () -> connection.unwrap(StorageConnector.class));
    }

    @Test
    public void containsCallsAdapterWithArguments() throws Exception {
        final Axiom<?> axiom = new AxiomImpl<>(SUBJECT, Assertion.createClassAssertion(false),
                new Value<>(NamedResource.create(Generator.generateUri())));
        final URI context = Generator.generateUri();
        connection.contains(axiom, Collections.singleton(context));
        verify(adapterMock).contains(axiom, Collections.singleton(context));
    }

    @Test
    public void containsThrowsIllegalStateExceptionForClosedConnection() throws Exception {
        final Axiom<?> axiom = new AxiomImpl<>(SUBJECT, Assertion.createClassAssertion(false),
                new Value<>(NamedResource.create(Generator.generateUri())));
        final URI context = Generator.generateUri();
        connection.close();
        assertThrows(IllegalStateException.class, () -> connection.contains(axiom, Collections.singleton(context)));
    }

    @Test
    public void getContextsCallsAdapter() {
        final List<URI> contexts = Collections.singletonList(Generator.generateUri());
        when(adapterMock.getContext()).thenReturn(contexts);
        final List<URI> result = connection.getContexts();
        verify(adapterMock).getContext();
        assertEquals(contexts, result);
    }

    @Test
    public void getContextsThrowsIllegalStateExceptionForClosedConnection() throws Exception {
        connection.close();
        try {
            assertThrows(IllegalStateException.class, () -> connection.getContexts());
        } finally {
            verify(adapterMock, never()).getContext();
        }
    }

    @Test
    public void generateIdentifierCallsAdapter() {
        final URI uri = Generator.generateUri();
        when(adapterMock.generateIdentifier(uri)).thenReturn(uri);
        final URI result = connection.generateIdentifier(uri);
        assertNotNull(result);
        verify(adapterMock).generateIdentifier(uri);
    }

    @Test
    public void generateIdentifierThrowsIllegalStateExceptionForClosedConnection() throws Exception {
        connection.close();
        assertThrows(IllegalStateException.class, () -> connection.generateIdentifier(Generator.generateUri()));
    }

    @Test
    public void findCallsAdapterWithDescriptor() throws Exception {
        final AxiomDescriptor descriptor = new AxiomDescriptor(SUBJECT);
        connection.find(descriptor);
        verify(adapterMock).find(descriptor);
    }

    @Test
    public void findThrowsIllegalStateExceptionForClosedConnection() throws Exception {
        connection.close();
        final AxiomDescriptor descriptor = new AxiomDescriptor(SUBJECT);
        assertThrows(IllegalStateException.class, () -> connection.find(descriptor));
    }

    @Test
    public void typesReturnsTypesHandler() {
        final JenaTypes types = connection.types();
        assertNotNull(types);
    }

    @Test
    public void removeCallsAdapterWithDescriptor() throws Exception {
        final AxiomDescriptor descriptor = new AxiomDescriptor(SUBJECT);
        connection.remove(descriptor);
        verify(adapterMock).remove(descriptor);
    }

    @Test
    public void removeCommitsTransactionWhenConnectionIsAutoCommit() throws Exception {
        connection.setAutoCommit(true);
        final AxiomDescriptor descriptor = new AxiomDescriptor(SUBJECT);
        connection.remove(descriptor);
        final InOrder inOrder = inOrder(adapterMock);
        inOrder.verify(adapterMock).remove(descriptor);
        inOrder.verify(adapterMock).commit();
    }

    @Test
    public void updatePassesDataToAdapter() throws Exception {
        final AxiomValueDescriptor descriptor = new AxiomValueDescriptor(SUBJECT);
        connection.update(descriptor);
        verify(adapterMock).update(descriptor);
    }

    @Test
    public void updateCommitsTransactionWhenConnectionIsAutoCommit() throws Exception {
        final AxiomValueDescriptor descriptor = new AxiomValueDescriptor(SUBJECT);
        connection.setAutoCommit(true);
        connection.update(descriptor);
        final InOrder inOrder = inOrder(adapterMock);
        inOrder.verify(adapterMock).update(descriptor);
        inOrder.verify(adapterMock).commit();
    }

    @Test
    public void updateThrowsIllegalStateExceptionForClosedConnection() throws Exception {
        connection.close();
        assertThrows(IllegalStateException.class, () -> connection.update(new AxiomValueDescriptor(SUBJECT)));
    }

    @Test
    public void propertiesReturnsPropertiesHandler() {
        final JenaProperties result = connection.properties();
        assertNotNull(result);
    }

    @Test
    public void listsReturnsListHandler() {
        final JenaLists result = connection.lists();
        assertNotNull(result);
    }

    @Test
    public void createStatementReturnsJenaStatement() {
        connection.createStatement();
        verify(adapterMock).createStatement();
    }

    @Test
    public void createStatementThrowsIllegalStateExceptionForClosedConnection() throws Exception {
        connection.close();
        assertThrows(IllegalStateException.class, () -> connection.createStatement());
    }

    @Test
    public void prepareStatementCreatesPreparedStatement() {
        final String query = "SELECT * WHERE { ?x ?y ?z . }";
        connection.prepareStatement(query);
        verify(adapterMock).prepareStatement(query);
    }

    @Test
    public void prepareStatementThrowsIllegalStateExceptionForClosedConnection() throws Exception {
        connection.close();
        assertThrows(IllegalStateException.class, () -> connection.prepareStatement("SELECT * WHERE { ?x ?y ?z . }"));
    }

    @Test
    public void isConsistentInvokesConsistencyCheckOnAdapter() {
        connection.isConsistent(null);
        verify(adapterMock).isConsistent(null);
    }

    @Test
    public void isConsistentThrowsIllegalStateExceptionForClosedConnection() throws Exception {
        connection.close();
        assertThrows(IllegalStateException.class, () -> connection.isConsistent(Generator.generateUri()));
    }
}

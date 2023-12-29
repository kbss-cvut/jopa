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

import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.PreparedStatement;
import cz.cvut.kbss.ontodriver.Statement;
import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.AxiomValueDescriptor;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.rdf4j.environment.Generator;
import cz.cvut.kbss.ontodriver.rdf4j.query.Rdf4jPreparedStatement;
import cz.cvut.kbss.ontodriver.rdf4j.query.Rdf4jStatement;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.net.URI;
import java.util.*;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
public class Rdf4jConnectionTest {

    @Mock
    private Rdf4jAdapter adapterMock;

    private Connection connection;

    @BeforeEach
    public void setUp() {
        this.connection = new Rdf4jConnection(adapterMock);
    }

    @Test
    public void testClose() throws Exception {
        assertTrue(connection.isOpen());
        connection.close();
        assertFalse(connection.isOpen());
        connection.close();
        assertFalse(connection.isOpen());
        verify(adapterMock).close();
    }

    @Test
    public void testCommit() throws Exception {
        connection.commit();
        verify(adapterMock).commit();
    }

    @Test
    public void testCommitClosed() throws Exception {
        connection.close();
        assertFalse(connection.isOpen());
        assertThrows(IllegalStateException.class, () -> connection.commit());
        verify(adapterMock, never()).commit();
    }

    @Test
    public void testRollback() throws Exception {
        connection.rollback();
        verify(adapterMock).rollback();
    }

    @Test
    public void testRollbackAutoCommit() throws Exception {
        connection.setAutoCommit(true);
        connection.rollback();
        verify(adapterMock, never()).rollback();
    }

    @Test
    public void testSetAutoCommit() {
        connection.setAutoCommit(true);
        assertTrue(connection.isAutoCommit());
        connection.setAutoCommit(false);
        assertFalse(connection.isAutoCommit());
    }

    @Test
    public void testCreateStatement() throws Exception {
        final Statement res = connection.createStatement();
        assertNotNull(res);
        assertTrue(res instanceof Rdf4jStatement);
    }

    @Test
    public void testCreateStatementOnClosed() throws Exception {
        connection.close();
        assertThrows(IllegalStateException.class, () -> connection.createStatement());
    }

    @Test
    public void testPrepareStatement() throws Exception {
        final PreparedStatement res = connection
                .prepareStatement("SELECT ?x ? y ?z WHERE { ?x ?y ?z . }");
        assertNotNull(res);
        assertTrue(res instanceof Rdf4jPreparedStatement);
    }

    @Test
    public void testPrepareStatementEmpty() {
        assertThrows(IllegalArgumentException.class, () -> connection.prepareStatement(""));
    }

    @Test
    public void testIsConsistent() throws Exception {
        final URI ctx = URI.create("http://contextUri");
        when(adapterMock.isConsistent(ctx)).thenReturn(Boolean.TRUE);
        boolean res = connection.isConsistent(ctx);
        assertTrue(res);
        verify(adapterMock).isConsistent(ctx);
    }

    @Test
    public void testGetContexts() throws Exception {
        final List<URI> contexts = new ArrayList<>();
        contexts.add(URI.create("http://contextOne"));
        contexts.add(URI.create("http://contextTwo"));
        when(adapterMock.getContexts()).thenReturn(contexts);
        final List<URI> res = connection.getContexts();
        assertEquals(contexts, res);
        verify(adapterMock).getContexts();
    }

    @Test
    public void testGetContextsOnClosed() throws Exception {
        connection.close();
        assertThrows(IllegalStateException.class, () -> connection.getContexts());
        verify(adapterMock, never()).getContexts();
    }

    @Test
    public void testGenerateIdentifier() throws Exception {
        final URI classUri = URI.create("https://onto.fel.cvut.cz/ontologies/jopa/owlClassA");
        final URI identifier = Generator.generateUri();
        when(adapterMock.generateIdentifier(classUri)).thenReturn(identifier);
        final URI res = connection.generateIdentifier(classUri);
        assertEquals(identifier, res);
        verify(adapterMock).generateIdentifier(classUri);
    }

    @Test
    public void testContainsAxiom() throws Exception {
        final Axiom<?> ax = mock(Axiom.class);
        when(adapterMock.contains(eq(ax), anySet())).thenReturn(Boolean.TRUE);
        final boolean res = connection.contains(ax, Collections.emptySet());
        assertTrue(res);
        verify(adapterMock).contains(ax, Collections.emptySet());
    }

    @Test
    public void testContainsAxiomInContext() throws Exception {
        final Axiom<?> ax = mock(Axiom.class);
        final Set<URI> context = Collections.singleton(URI.create("http://context.org"));
        when(adapterMock.contains(ax, context)).thenReturn(Boolean.TRUE);
        final boolean res = connection.contains(ax, context);
        assertTrue(res);
        verify(adapterMock).contains(ax, context);
    }

    @Test
    public void testFind() throws Exception {
        final AxiomDescriptor axDesc = mock(AxiomDescriptor.class);
        final List<Axiom<?>> axioms = new ArrayList<>();
        axioms.add(mock(Axiom.class));
        axioms.add(mock(Axiom.class));
        when(adapterMock.find(axDesc)).thenReturn(axioms);
        final Collection<Axiom<?>> res = connection.find(axDesc);
        assertEquals(axioms, res);
    }

    @Test
    public void testFindNull() throws Exception {
        assertThrows(NullPointerException.class, () -> connection.find(null));
        verify(adapterMock, never()).find(any(AxiomDescriptor.class));
    }

    @Test
    public void testPersist() throws Exception {
        final AxiomValueDescriptor axDesc = mock(AxiomValueDescriptor.class);
        connection.persist(axDesc);
        verify(adapterMock).persist(axDesc);
    }

    @Test
    public void testPersistAutoCommit() throws Exception {
        final AxiomValueDescriptor axDesc = mock(AxiomValueDescriptor.class);
        connection.setAutoCommit(true);
        connection.persist(axDesc);
        verify(adapterMock).persist(axDesc);
        verify(adapterMock).commit();
    }

    @Test
    public void testPersistOnClosed() throws Exception {
        connection.close();
        final AxiomValueDescriptor axDesc = mock(AxiomValueDescriptor.class);
        assertThrows(IllegalStateException.class, () -> connection.persist(axDesc));
        verify(adapterMock, never()).persist(axDesc);
    }

    @Test
    public void testUpdate() throws Exception {
        final AxiomValueDescriptor axDesc = mock(AxiomValueDescriptor.class);
        connection.update(axDesc);
        verify(adapterMock).update(axDesc);
    }

    @Test
    public void testUpdateAutoCommit() throws Exception {
        final AxiomValueDescriptor axDesc = mock(AxiomValueDescriptor.class);
        connection.setAutoCommit(true);
        connection.update(axDesc);
        verify(adapterMock).update(axDesc);
        verify(adapterMock).commit();
    }

    @Test
    public void testRemove() throws Exception {
        final AxiomDescriptor axDesc = mock(AxiomDescriptor.class);
        connection.remove(axDesc);
        verify(adapterMock).remove(axDesc);
    }

    @Test
    public void unwrapReturnsConnectionIfItMatchesTheClass() throws Exception {
        assertSame(connection, connection.unwrap(Rdf4jConnection.class));
    }

    @Test
    public void unwrapPassesCallAlongWhenConnectionDoesNotMatch() throws Exception {
        connection.unwrap(Rdf4jAdapter.class);
        verify(adapterMock).unwrap(Rdf4jAdapter.class);
    }
}

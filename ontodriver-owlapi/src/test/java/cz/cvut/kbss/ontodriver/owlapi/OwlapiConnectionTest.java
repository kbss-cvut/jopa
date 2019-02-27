/**
 * Copyright (C) 2019 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.owlapi;

import cz.cvut.kbss.ontodriver.owlapi.list.OwlapiLists;
import cz.cvut.kbss.ontodriver.Lists;
import cz.cvut.kbss.ontodriver.Properties;
import cz.cvut.kbss.ontodriver.Types;
import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.model.*;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.net.URI;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import static org.junit.Assert.*;
import static org.mockito.Mockito.*;

public class OwlapiConnectionTest {

    @Mock
    private OwlapiAdapter adapterMock;

    private OwlapiConnection connection;

    @Before
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        this.connection = new OwlapiConnection(adapterMock);
    }

    @Test
    public void commitCommitsAdapterChanges() {
        connection.commit();
        verify(adapterMock).commit();
    }

    @Test(expected = IllegalStateException.class)
    public void commitOnCloseThrowsIllegalState() {
        connection.close();
        connection.commit();
    }

    @Test
    public void closeConnectionNotifiesListeners() {
        final ConnectionListener listener = mock(ConnectionListener.class);
        connection.setListener(listener);
        assertTrue(connection.isOpen());
        connection.close();
        assertFalse(connection.isOpen());
        verify(listener).connectionClosed(connection);
    }

    @Test
    public void rollbackRollsBackAdapter() {
        connection.rollback();
        verify(adapterMock).rollback();
    }

    @Test
    public void testIsConsistentWithCorrectContextUri() {
        final URI ctx = URI.create("http://context.owl");
        when(adapterMock.isConsistent(ctx)).thenReturn(Boolean.TRUE);
        assertTrue(connection.isConsistent(ctx));
        verify(adapterMock).isConsistent(ctx);
    }

    @Test
    public void testGetContexts() {
        final List<URI> contexts = Collections.singletonList(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa"));
        when(adapterMock.getContexts()).thenReturn(contexts);
        final List<URI> res = connection.getContexts();
        assertSame(contexts, res);
    }

    @Test
    public void testContainsWithContext() {
        final Axiom<URI> axiom = new AxiomImpl<>(NamedResource.create("http://individual"),
                Assertion.createClassAssertion(false), new Value<>(URI.create("http://class")));
        final URI context = URI.create("http://krizik.felk.cvut.cz/ontologies/jopa");
        connection.contains(axiom, context);
        verify(adapterMock).containsAxiom(axiom, context);
    }

    @Test
    public void testContainsWithoutContext() {
        final Axiom<URI> axiom = new AxiomImpl<>(NamedResource.create("http://individual"),
                Assertion.createClassAssertion(false), new Value<>(URI.create("http://class")));
        connection.contains(axiom, null);
        verify(adapterMock).containsAxiom(axiom, null);
    }

    @Test
    public void testFind() throws Exception {
        final AxiomDescriptor descriptor = new AxiomDescriptor(
                NamedResource.create("http://krizik.felk.cvut.cz/ontologies/jopa#instance"));
        final Collection<Axiom<?>> axioms = Collections.emptyList();
        when(adapterMock.find(descriptor)).thenReturn(axioms);

        final Collection<Axiom<?>> res = connection.find(descriptor);
        assertSame(axioms, res);
        verify(adapterMock).find(descriptor);
    }

    @Test(expected = IllegalStateException.class)
    public void findOnCloseThrowsIllegalState() throws Exception {
        final AxiomDescriptor descriptor = new AxiomDescriptor(
                NamedResource.create("http://krizik.felk.cvut.cz/ontologies/jopa#instance"));
        connection.close();
        connection.find(descriptor);
    }

    @Test
    public void typesReturnsTypesHandlerForOwlapiDriver() {
        final OwlapiTypes types = mock(OwlapiTypes.class);
        connection.setTypes(types);
        final Types result = connection.types();
        assertNotNull(result);
        assertEquals(types, result);
    }

    @Test
    public void generateIdentifierGetsNewIdentifier() {
        final URI identifier = URI.create("http://newIdentifier");
        when(adapterMock.generateIdentifier(any(URI.class))).thenReturn(identifier);

        final URI result = connection.generateIdentifier(URI.create("http://baseUri"));
        assertEquals(identifier, result);
    }

    @Test(expected = IllegalStateException.class)
    public void generateIdentifierOnClosedThrowsException() {
        connection.close();
        try {
            connection.generateIdentifier(URI.create("http://baseUri"));
        } finally {
            verify(adapterMock, never()).generateIdentifier(any(URI.class));
        }
    }

    @Test
    public void propertiesReturnPropertiesHandlerForOwlapiDriver() {
        final OwlapiProperties properties = mock(OwlapiProperties.class);
        connection.setProperties(properties);
        final Properties result = connection.properties();
        assertNotNull(result);
        assertEquals(properties, result);
    }

    @Test
    public void listsReturnListsHandlerForOwlapiDriver() {
        final OwlapiLists lists = mock(OwlapiLists.class);
        connection.setLists(lists);
        final Lists result = connection.lists();
        assertNotNull(result);
        assertEquals(lists, result);
    }
}
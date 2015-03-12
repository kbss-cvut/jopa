package cz.cvut.kbss.ontodriver.owlapi;

import cz.cvut.kbss.ontodriver_new.model.*;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.net.URI;
import java.util.Collections;
import java.util.List;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
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
    public void commitCommitsAdapterChanges() throws Exception {
        connection.commit();
        verify(adapterMock).commit();
    }

    @Test(expected = IllegalStateException.class)
    public void commitOnCloseThrowsIllegalState() throws Exception {
        connection.close();
        connection.commit();
    }

    @Test
    public void closeConnectionNotifiesListeners() throws Exception {
        final ConnectionListener listener = mock(ConnectionListener.class);
        connection.addListener(listener);
        assertTrue(connection.isOpen());
        connection.close();
        assertFalse(connection.isOpen());
        verify(listener).connectionClosed(connection);
    }

    @Test
    public void rollbackRollsBackAdapter() throws Exception {
        connection.rollback();
        verify(adapterMock).rollback();
    }

    @Test
    public void testIsConsistentWithCorrectContextUri() throws Exception {
        final URI ctx = URI.create("http://context.owl");
        when(adapterMock.isConsistent(ctx)).thenReturn(Boolean.TRUE);
        assertTrue(connection.isConsistent(ctx));
        verify(adapterMock).isConsistent(ctx);
    }

    @Test
    public void testGetContexts() throws Exception {
        final List<URI> contexts = Collections.singletonList(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa"));
        when(adapterMock.getContexts()).thenReturn(contexts);
        final List<URI> res = connection.getContexts();
        assertSame(contexts, res);
    }

    @Test
    public void testContainsWithContext() throws Exception {
        final Axiom<URI> axiom = new AxiomImpl<>(NamedResource.create("http://individual"),
                Assertion.createClassAssertion(false), new Value<>(URI.create("http://class")));
        final URI context = URI.create("http://krizik.felk.cvut.cz/ontologies/jopa");
        connection.contains(axiom, context);
        verify(adapterMock).containsAxiom(axiom, context);
    }

    @Test
    public void testContainsWithoutContext() throws Exception {
        final Axiom<URI> axiom = new AxiomImpl<>(NamedResource.create("http://individual"),
                Assertion.createClassAssertion(false), new Value<>(URI.create("http://class")));
        connection.contains(axiom, null);
        verify(adapterMock).containsAxiom(axiom, null);
    }
}
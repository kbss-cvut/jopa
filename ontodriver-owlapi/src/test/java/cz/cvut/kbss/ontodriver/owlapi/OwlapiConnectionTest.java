package cz.cvut.kbss.ontodriver.owlapi;

import cz.cvut.kbss.ontodriver_new.Connection;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import static org.junit.Assert.*;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

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
}
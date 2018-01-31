package cz.cvut.kbss.ontodriver.jena;

import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.AxiomValueDescriptor;
import cz.cvut.kbss.ontodriver.jena.connector.StorageConnector;
import cz.cvut.kbss.ontodriver.jena.environment.Generator;
import cz.cvut.kbss.ontodriver.jena.util.ConnectionListener;
import cz.cvut.kbss.ontodriver.model.*;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.mockito.InOrder;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import java.net.URI;
import java.util.Collections;
import java.util.List;

import static org.hamcrest.CoreMatchers.containsString;
import static org.junit.Assert.*;
import static org.mockito.Mockito.*;

public class JenaConnectionTest {

    private static final NamedResource SUBJECT = NamedResource.create(Generator.generateUri());

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Mock
    private JenaAdapter adapterMock;

    private JenaConnection connection;

    @Before
    public void setUp() {
        MockitoAnnotations.initMocks(this);
        this.connection = new JenaConnection(adapterMock);
    }

    @Test
    public void setAutoCommitThrowsIllegalStateForClosedConnection() throws Exception {
        connection.close();
        expectClosedException();
        connection.setAutoCommit(false);
    }

    private void expectClosedException() {
        thrown.expect(IllegalStateException.class);
        thrown.expectMessage(containsString("connection is closed"));
    }

    @Test
    public void isAutoCommitThrowsIllegalStateForClosedConnection() throws Exception {
        connection.close();
        expectClosedException();
        connection.isAutoCommit();
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
        expectClosedException();
        connection.commit();
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
        expectClosedException();
        connection.rollback();
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
        expectClosedException();
        connection.persist(descriptor);
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
        expectClosedException();
        connection.unwrap(StorageConnector.class);
    }

    @Test
    public void containsCallsAdapterWithArguments() throws Exception {
        final Axiom<?> axiom = new AxiomImpl<>(SUBJECT, Assertion.createClassAssertion(false),
                new Value<>(NamedResource.create(Generator.generateUri())));
        final URI context = Generator.generateUri();
        connection.contains(axiom, context);
        verify(adapterMock).contains(axiom, context);
    }

    @Test
    public void containsThrowsIllegalStateExceptionForClosedConnection() throws Exception {
        final Axiom<?> axiom = new AxiomImpl<>(SUBJECT, Assertion.createClassAssertion(false),
                new Value<>(NamedResource.create(Generator.generateUri())));
        final URI context = Generator.generateUri();
        connection.close();
        expectClosedException();
        connection.contains(axiom, context);
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
        expectClosedException();
        try {
            connection.getContexts();
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
        expectClosedException();
        connection.generateIdentifier(Generator.generateUri());
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
        expectClosedException();
        final AxiomDescriptor descriptor = new AxiomDescriptor(SUBJECT);
        connection.find(descriptor);
    }
}
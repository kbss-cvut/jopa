package cz.cvut.kbss.ontodriver.jena;

import cz.cvut.kbss.ontodriver.descriptor.AxiomValueDescriptor;
import cz.cvut.kbss.ontodriver.jena.connector.StorageConnector;
import cz.cvut.kbss.ontodriver.jena.environment.Generator;
import cz.cvut.kbss.ontodriver.model.*;
import org.apache.jena.rdf.model.Statement;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.net.URI;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyListOf;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class JenaAdapterTest {

    private static final NamedResource SUBJECT = NamedResource.create(Generator.generateUri());

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Mock
    private StorageConnector connectorMock;

    private JenaAdapter adapter;

    @Before
    public void setUp() {
        MockitoAnnotations.initMocks(this);
        this.adapter = new JenaAdapter(connectorMock);
    }

    @Test
    public void persistBeginsTransaction() {
        final AxiomValueDescriptor descriptor = new AxiomValueDescriptor(SUBJECT);
        adapter.persist(descriptor);
        verify(connectorMock).begin();
    }

    @Test
    public void persistGeneratesStatementsAndPassesThemToConnectorForPersist() {
        final AxiomValueDescriptor descriptor = new AxiomValueDescriptor(SUBJECT);
        final Assertion a = Assertion.createClassAssertion(false);
        final NamedResource type = NamedResource.create(Generator.generateUri());
        descriptor.addAssertionValue(a, new Value<>(type));
        adapter.persist(descriptor);
        final ArgumentCaptor<List> captor = ArgumentCaptor.forClass(List.class);
        verify(connectorMock).add(captor.capture());
        final List<Statement> arg = captor.getValue();
        assertEquals(1, arg.size());
    }

    @Test
    public void commitDataToStorage() throws Exception {
        final AxiomValueDescriptor descriptor = new AxiomValueDescriptor(SUBJECT);
        final Assertion a = Assertion.createClassAssertion(false);
        final NamedResource type = NamedResource.create(Generator.generateUri());
        descriptor.addAssertionValue(a, new Value<>(type));
        adapter.persist(descriptor);
        adapter.commit();
        verify(connectorMock).add(anyListOf(Statement.class));
        verify(connectorMock).commit();
    }

    @Test
    public void commitDoesNothingWhenTransactionIsNotActive() throws Exception {
        adapter.commit();
        verify(connectorMock, never()).commit();
    }

    @Test
    public void rollbackRollsBackChanges() throws Exception {
        final AxiomValueDescriptor descriptor = new AxiomValueDescriptor(SUBJECT);
        final Assertion a = Assertion.createClassAssertion(false);
        final NamedResource type = NamedResource.create(Generator.generateUri());
        descriptor.addAssertionValue(a, new Value<>(type));
        adapter.persist(descriptor);
        adapter.rollback();
        verify(connectorMock).add(anyListOf(Statement.class));
        verify(connectorMock).rollback();
        verify(connectorMock, never()).commit();
    }

    @Test
    public void rollbackDoesNothingWhenTransactionIsNotActive() {
        adapter.rollback();
        verify(connectorMock, never()).rollback();
    }

    @Test
    public void closeClosesConnector() throws Exception {
        adapter.close();
        verify(connectorMock).close();
    }

    @Test
    public void containsStartsTransactionWhenItIsNotActive() {
        final String typeUri = Generator.generateUri().toString();
        final Axiom<?> ax = new AxiomImpl<>(SUBJECT, Assertion.createClassAssertion(false),
                new Value<>(NamedResource.create(typeUri)));
        adapter.contains(ax, Generator.generateUri());
        verify(connectorMock).begin();
    }

    @Test
    public void containsChecksForStatementExistence() {
        final String typeUri = Generator.generateUri().toString();
        final Axiom<?> ax = new AxiomImpl<>(SUBJECT, Assertion.createClassAssertion(false),
                new Value<>(NamedResource.create(typeUri)));
        when(connectorMock.contains(any(), any(), any())).thenReturn(true);
        assertTrue(adapter.contains(ax, null));
    }

    @Test
    public void getContextsListsContextsFromConnectorAndTransformsThemToUris() {
        final List<String> contexts = IntStream.range(0, 5).mapToObj(i -> Generator.generateUri().toString()).collect(
                Collectors.toList());
        when(connectorMock.getContexts()).thenReturn(contexts);
        final List<URI> result = adapter.getContext();
        assertEquals(contexts.size(), result.size());
        result.forEach(u -> assertTrue(contexts.contains(u.toString())));
    }

    @Test
    public void unwrapReturnsAdapterInstanceWhenTargetClassMatches() throws Exception {
        final JenaAdapter result = adapter.unwrap(JenaAdapter.class);
        assertSame(adapter, result);
    }
}
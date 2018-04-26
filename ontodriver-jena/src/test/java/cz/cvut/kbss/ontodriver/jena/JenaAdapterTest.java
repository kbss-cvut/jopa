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
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
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

import static org.junit.Assert.*;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyListOf;
import static org.mockito.Mockito.*;

public class JenaAdapterTest {

    private static final NamedResource SUBJECT = NamedResource.create(Generator.generateUri());
    private static final Resource SUBJECT_RESOURCE = ResourceFactory.createResource(SUBJECT.getIdentifier().toString());

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Mock
    private StorageConnector connectorMock;

    @Mock
    private InferredStorageConnector inferredConnectorMock;

    private JenaAdapter adapter;

    @Before
    public void setUp() {
        MockitoAnnotations.initMocks(this);
        this.adapter = new JenaAdapter(connectorMock, inferredConnectorMock);
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
        verify(connectorMock).add(captor.capture(), eq(null));
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
        verify(connectorMock).add(anyListOf(Statement.class), eq(null));
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
        verify(connectorMock).add(anyListOf(Statement.class), eq(null));
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
        when(connectorMock.contains(any(), any(), any(), anyString())).thenReturn(true);
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

    @Test
    public void unwrapPassesCallToStorageConnectorWhenTargetClassDoesNotMatch() throws Exception {
        adapter.unwrap(StorageConnector.class);
        verify(connectorMock).unwrap(StorageConnector.class);
    }

    @Test
    public void findLoadsAxiomsFromStorage() {
        final AxiomDescriptor descriptor = new AxiomDescriptor(SUBJECT);
        final Assertion assertion = Assertion.createObjectPropertyAssertion(Generator.generateUri(), false);
        descriptor.addAssertion(assertion);
        final Statement s = ResourceFactory
                .createStatement(SUBJECT_RESOURCE, assertionToProperty(assertion),
                        ResourceFactory.createResource(Generator.generateUri().toString()));
        when(connectorMock.find(any(), any(), any(), anyString())).thenReturn(Collections.singletonList(s));

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
    public void removeRemovesStatementsFromStorage() {
        final AxiomDescriptor descriptor = new AxiomDescriptor(SUBJECT);
        final Assertion assertion = Assertion.createObjectPropertyAssertion(Generator.generateUri(), false);
        descriptor.addAssertion(assertion);

        adapter.remove(descriptor);
        verify(connectorMock).remove(SUBJECT_RESOURCE, assertionToProperty(assertion), null, null);
    }

    @Test
    public void updateRemovesOldStatementsAndInsertsNewOnes() {
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
    public void createStatementReturnsNewJenaStatement() throws Exception {
        final JenaStatement result = adapter.createStatement();
        assertNotNull(result);
        final Field execField = JenaStatement.class.getDeclaredField("executor");
        execField.setAccessible(true);
        assertSame(connectorMock, execField.get(result));
    }

    @Test
    public void createStatementStartsTransactionIfNotAlreadyActive() {
        adapter.createStatement();
        verify(connectorMock).begin();
    }

    @Test
    public void prepareStatementReturnsNewPreparedStatement() throws Exception {
        final JenaPreparedStatement result = adapter.prepareStatement("SELECT * WHERE {?x ?y ?z . }");
        assertNotNull(result);
        final Field execField = JenaStatement.class.getDeclaredField("executor");
        execField.setAccessible(true);
        assertSame(connectorMock, execField.get(result));
    }

    @Test
    public void prepareStatementStartsTransactionIfNotAlreadyActive() {
        adapter.prepareStatement("SELECT * WHERE {?x ?y ?z . }");
        verify(connectorMock).begin();
    }
}
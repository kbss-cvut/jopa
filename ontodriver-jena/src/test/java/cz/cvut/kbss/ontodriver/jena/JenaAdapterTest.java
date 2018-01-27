package cz.cvut.kbss.ontodriver.jena;

import cz.cvut.kbss.ontodriver.descriptor.AxiomValueDescriptor;
import cz.cvut.kbss.ontodriver.jena.connector.StorageConnector;
import cz.cvut.kbss.ontodriver.jena.environment.Generator;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;
import org.apache.jena.rdf.model.Statement;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.List;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.verify;

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
}
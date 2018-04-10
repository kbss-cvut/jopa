package cz.cvut.kbss.ontodriver.jena.list;

import cz.cvut.kbss.ontodriver.descriptor.ListDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.ListValueDescriptor;
import cz.cvut.kbss.ontodriver.jena.connector.StorageConnector;
import cz.cvut.kbss.ontodriver.jena.environment.Generator;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.Statement;
import org.junit.Test;
import org.mockito.Mock;

import java.net.URI;
import java.util.List;
import java.util.stream.Collectors;

import static org.apache.jena.rdf.model.ResourceFactory.createProperty;
import static org.apache.jena.rdf.model.ResourceFactory.createResource;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.mockito.Matchers.anyListOf;
import static org.mockito.Matchers.anyString;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

abstract class ListHandlerTestBase<D extends ListDescriptor, V extends ListValueDescriptor> {

    static final NamedResource OWNER = NamedResource.create(Generator.generateUri());
    static final Assertion HAS_LIST = Assertion.createObjectPropertyAssertion(Generator.generateUri(), false);
    static final Assertion HAS_NEXT = Assertion.createObjectPropertyAssertion(Generator.generateUri(), false);
    static final Resource OWNER_RESOURCE = createResource(OWNER.getIdentifier().toString());
    static final Property HAS_LIST_PROPERTY = createProperty(HAS_LIST.getIdentifier().toString());
    static final Property HAS_NEXT_PROPERTY = createProperty(HAS_NEXT.getIdentifier().toString());

    @Mock
    StorageConnector connectorMock;

    ListTestUtil listUtil;

    ListHandler<D, V> handler;

    public void setUp() {
        this.listUtil = new ListTestUtil(OWNER_RESOURCE, HAS_LIST_PROPERTY, HAS_NEXT_PROPERTY, connectorMock);
    }

    @Test
    public void loadListRetrievesAllListElements() {
        final List<URI> expected = generateList(null);
        final List<Axiom<NamedResource>> result = handler.loadList(listDescriptor());
        assertNotNull(result);
        final List<URI> actual = result.stream().map(ax -> ax.getValue().getValue().getIdentifier())
                                       .collect(Collectors.toList());
        assertEquals(expected, actual);
    }

    abstract List<URI> generateList(String context);

    abstract D listDescriptor();

    @Test
    public void loadListFromContextRetrievesAllListElements() {
        final URI context = Generator.generateUri();
        final List<URI> expected = generateList(context.toString());
        final D descriptor = listDescriptor();
        descriptor.setContext(context);
        final List<Axiom<NamedResource>> result = handler.loadList(descriptor);
        final List<URI> actual = result.stream().map(ax -> ax.getValue().getValue().getIdentifier())
                                       .collect(Collectors.toList());
        assertEquals(expected, actual);
    }

    abstract V listValueDescriptor();

    @Test
    public void persistListDoesNothingForEmptyDescriptor() {
        final V descriptor = listValueDescriptor();
        handler.persistList(descriptor);
        verify(connectorMock, never()).add(anyListOf(Statement.class), anyString());
    }
}

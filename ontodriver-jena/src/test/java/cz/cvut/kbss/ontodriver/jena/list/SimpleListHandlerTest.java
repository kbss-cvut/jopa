package cz.cvut.kbss.ontodriver.jena.list;

import cz.cvut.kbss.ontodriver.descriptor.SimpleListDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.SimpleListDescriptorImpl;
import cz.cvut.kbss.ontodriver.jena.connector.StorageConnector;
import cz.cvut.kbss.ontodriver.jena.environment.Generator;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.Resource;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.net.URI;
import java.util.List;
import java.util.stream.Collectors;

import static org.apache.jena.rdf.model.ResourceFactory.createProperty;
import static org.apache.jena.rdf.model.ResourceFactory.createResource;
import static org.junit.Assert.assertEquals;

public class SimpleListHandlerTest {

    private static final NamedResource RESOURCE = NamedResource.create(Generator.generateUri());
    private static final Assertion HAS_LIST = Assertion.createObjectPropertyAssertion(Generator.generateUri(), false);
    private static final Assertion HAS_NEXT = Assertion.createObjectPropertyAssertion(Generator.generateUri(), false);

    @Mock
    private StorageConnector connectorMock;

    private SimpleListHandler handler;

    @Before
    public void setUp() {
        MockitoAnnotations.initMocks(this);
        this.handler = new SimpleListHandler(connectorMock);
    }

    @Test
    public void loadListRetrievesAllListElements() {
        final List<URI> expected = generateList(null);
        final List<Axiom<NamedResource>> result = handler
                .loadList(new SimpleListDescriptorImpl(RESOURCE, HAS_LIST, HAS_NEXT));
        final List<URI> actual = result.stream().map(ax -> ax.getValue().getValue().getIdentifier())
                                       .collect(Collectors.toList());
        assertEquals(expected, actual);
    }

    private List<URI> generateList(String context) {
        final Property hasList = createProperty(HAS_LIST.getIdentifier().toString());
        final Property hasNext = createProperty(HAS_NEXT.getIdentifier().toString());
        final Resource owner = createResource(RESOURCE.getIdentifier().toString());
        if (context != null) {
            return ListTestUtil.generateSimpleList(owner, hasList, hasNext, connectorMock, context);
        } else {
            return ListTestUtil.generateSimpleList(owner, hasList, hasNext, connectorMock);
        }
    }

    @Test
    public void loadListFromContextRetrievesAllListElements() {
        final URI context = Generator.generateUri();
        final List<URI> expected = generateList(context.toString());
        final SimpleListDescriptor descriptor = new SimpleListDescriptorImpl(RESOURCE, HAS_LIST, HAS_NEXT);
        descriptor.setContext(context);
        final List<Axiom<NamedResource>> result = handler.loadList(descriptor);
        final List<URI> actual = result.stream().map(ax -> ax.getValue().getValue().getIdentifier())
                                       .collect(Collectors.toList());
        assertEquals(expected, actual);
    }
}
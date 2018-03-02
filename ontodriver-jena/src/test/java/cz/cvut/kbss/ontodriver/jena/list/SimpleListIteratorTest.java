package cz.cvut.kbss.ontodriver.jena.list;

import cz.cvut.kbss.ontodriver.exception.IntegrityConstraintViolatedException;
import cz.cvut.kbss.ontodriver.jena.connector.StorageConnector;
import cz.cvut.kbss.ontodriver.jena.environment.Generator;
import cz.cvut.kbss.ontodriver.jena.exception.ListProcessingException;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.ResourceFactory;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.net.URI;
import java.util.*;

import static org.apache.jena.rdf.model.ResourceFactory.createResource;
import static org.apache.jena.rdf.model.ResourceFactory.createStatement;
import static org.apache.jena.rdf.model.ResourceFactory.createTypedLiteral;
import static org.junit.Assert.*;
import static org.mockito.Mockito.when;

public class SimpleListIteratorTest {

    private static final Resource RESOURCE = createResource(Generator.generateUri().toString());
    private static final Property HAS_LIST = ResourceFactory.createProperty(Generator.generateUri().toString());
    private static final Property HAS_NEXT = ResourceFactory.createProperty(Generator.generateUri().toString());

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Mock
    private StorageConnector connectorMock;

    @Before
    public void setUp() {
        MockitoAnnotations.initMocks(this);
    }

    @Test
    public void hasNextReturnsTrueForListHead() {
        generateList();
        final SimpleListIterator iterator = new SimpleListIterator(RESOURCE, HAS_LIST, HAS_NEXT, null, connectorMock);
        assertTrue(iterator.hasNext());
    }

    private List<URI> generateList() {
        return ListTestUtil.generateSimpleList(RESOURCE, HAS_LIST, HAS_NEXT, connectorMock);
    }

    @Test
    public void nextReturnsFirstListElement() {
        final List<URI> list = generateList();
        final SimpleListIterator iterator = new SimpleListIterator(RESOURCE, HAS_LIST, HAS_NEXT, null, connectorMock);
        assertTrue(iterator.hasNext());
        final Axiom<NamedResource> head = iterator.next();
        assertNotNull(head);
        assertEquals(NamedResource.create(RESOURCE.getURI()), head.getSubject());
        assertEquals(Assertion.createObjectPropertyAssertion(URI.create(HAS_LIST.getURI()), false),
                head.getAssertion());
        assertEquals(NamedResource.create(list.get(0)), head.getValue().getValue());
    }

    @Test
    public void nextThrowsNoSuchElementWhenNoMoreElementsExist() {
        final SimpleListIterator iterator = new SimpleListIterator(RESOURCE, HAS_LIST, HAS_NEXT, null, connectorMock);
        assertFalse(iterator.hasNext());
        thrown.expect(NoSuchElementException.class);
        iterator.next();
    }

    @Test
    public void nextAllowsToReadWholeList() {
        final List<URI> list = generateList();
        final SimpleListIterator iterator = new SimpleListIterator(RESOURCE, HAS_LIST, HAS_NEXT, null, connectorMock);
        final List<URI> actual = new ArrayList<>();
        while (iterator.hasNext()) {
            final Axiom<NamedResource> axiom = iterator.next();
            if (!axiom.getSubject().getIdentifier().equals(URI.create(RESOURCE.getURI()))) {
                assertEquals(URI.create(HAS_NEXT.getURI()), axiom.getAssertion().getIdentifier());
            }
            actual.add(axiom.getValue().getValue().getIdentifier());
        }
        assertEquals(list, actual);
    }

    @Test
    public void nextThrowsIntegrityConstraintViolationWhenMultipleNextNodesAreFound() {
        generateList();
        when(connectorMock.find(RESOURCE, HAS_LIST, null)).thenReturn(
                Arrays.asList(createStatement(RESOURCE, HAS_LIST, createResource()),
                        createStatement(RESOURCE, HAS_LIST, createResource())));
        final SimpleListIterator iterator = new SimpleListIterator(RESOURCE, HAS_LIST, HAS_NEXT, null, connectorMock);
        thrown.expect(IntegrityConstraintViolatedException.class);
        thrown.expectMessage("Encountered multiple successors of list node " + RESOURCE.getURI());
        iterator.next();
    }

    @Test
    public void nextThrowsListProcessingExceptionWhenNodeIsLiteral() {
        final List<URI> list = generateList();
        final String nodeUri = list.get(list.size() - 1).toString();
        when(connectorMock.find(createResource(nodeUri), HAS_NEXT, null)).thenReturn(
                Collections.singletonList(createStatement(createResource(nodeUri), HAS_NEXT, createTypedLiteral(117))));
        final SimpleListIterator iterator = new SimpleListIterator(RESOURCE, HAS_LIST, HAS_NEXT, null, connectorMock);
        thrown.expect(ListProcessingException.class);
        thrown.expectMessage("Expected successor of node " + nodeUri + " to be a named resource.");
        while(iterator.hasNext()) {
            iterator.next();
        }
    }
}
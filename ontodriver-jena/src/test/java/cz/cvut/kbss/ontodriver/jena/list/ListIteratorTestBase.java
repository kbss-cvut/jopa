package cz.cvut.kbss.ontodriver.jena.list;

import cz.cvut.kbss.ontodriver.descriptor.ListDescriptor;
import cz.cvut.kbss.ontodriver.exception.IntegrityConstraintViolatedException;
import cz.cvut.kbss.ontodriver.jena.connector.StorageConnector;
import cz.cvut.kbss.ontodriver.jena.environment.Generator;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.ResourceFactory;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.mockito.Mock;

import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.NoSuchElementException;

import static org.apache.jena.rdf.model.ResourceFactory.createResource;
import static org.apache.jena.rdf.model.ResourceFactory.createStatement;
import static org.hamcrest.CoreMatchers.containsString;
import static org.junit.Assert.*;
import static org.mockito.Mockito.when;

public abstract class ListIteratorTestBase<T extends AbstractListIterator, D extends ListDescriptor> {

    static final Resource RESOURCE = createResource(Generator.generateUri().toString());
    static final Property HAS_LIST = ResourceFactory.createProperty(Generator.generateUri().toString());
    static final Property HAS_NEXT = ResourceFactory.createProperty(Generator.generateUri().toString());
    static final Property HAS_CONTENT = ResourceFactory.createProperty(Generator.generateUri().toString());

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Mock
    StorageConnector connectorMock;

    ListTestUtil testUtil;

    public void setUp() {
        this.testUtil = new ListTestUtil(RESOURCE, HAS_LIST, HAS_NEXT, connectorMock);
    }

    abstract T iterator();

    abstract D descriptor(String context);

    abstract List<URI> generateList();

    @Test
    public void hasNextReturnsTrueForListHead() {
        generateList();
        final AbstractListIterator iterator = iterator();
        assertTrue(iterator.hasNext());
    }

    @Test
    public void nextThrowsNoSuchElementWhenNoMoreElementsExist() {
        final AbstractListIterator iterator = iterator();
        assertFalse(iterator.hasNext());
        thrown.expect(NoSuchElementException.class);
        iterator.nextAxiom();
    }

    @Test
    public void nextAllowsToReadWholeList() {
        final List<URI> list = generateList();
        final AbstractListIterator iterator = iterator();
        final List<URI> actual = new ArrayList<>();
        while (iterator.hasNext()) {
            final Axiom<NamedResource> axiom = iterator.nextAxiom();
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
        final AbstractListIterator iterator = iterator();
        thrown.expect(IntegrityConstraintViolatedException.class);
        thrown.expectMessage("Encountered multiple successors of list node " + RESOURCE.getURI());
        iterator.nextAxiom();
    }

    @Test
    public void removeWithoutReconnectThrowsIllegalStateExceptionWhenNextWasNotCalledBefore() {
        thrown.expect(IllegalStateException.class);
        thrown.expectMessage(containsString("Cannot call remove before calling next."));
        generateList();
        final AbstractListIterator iterator = iterator();
        iterator.removeWithoutReconnect();
    }

    @Test
    public void removeWithoutReconnectThrowsIllegalStateExceptionWhenRemoveIsCalledTwiceOnElement() {
        thrown.expect(IllegalStateException.class);
        thrown.expectMessage(containsString("Cannot call remove multiple times on one element."));
        generateList();
        final AbstractListIterator iterator = iterator();
        iterator.nextValue();
        iterator.removeWithoutReconnect();
        iterator.removeWithoutReconnect();
    }
}

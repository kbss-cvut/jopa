package cz.cvut.kbss.ontodriver.jena.list;

import cz.cvut.kbss.ontodriver.descriptor.ReferencedListDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.ReferencedListDescriptorImpl;
import cz.cvut.kbss.ontodriver.exception.IntegrityConstraintViolatedException;
import cz.cvut.kbss.ontodriver.jena.environment.Generator;
import cz.cvut.kbss.ontodriver.jena.exception.ListProcessingException;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import org.apache.jena.rdf.model.Resource;
import org.junit.Before;
import org.junit.Test;
import org.mockito.MockitoAnnotations;

import java.net.URI;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static org.apache.jena.rdf.model.ResourceFactory.createResource;
import static org.apache.jena.rdf.model.ResourceFactory.createStatement;
import static org.apache.jena.rdf.model.ResourceFactory.createTypedLiteral;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.when;

public class ReferencedListIteratorTest extends ListIteratorTestBase<ReferencedListIterator, ReferencedListDescriptor> {

    @Before
    public void setUp() {
        MockitoAnnotations.initMocks(this);
        super.setUp();
        testUtil.setHasContent(HAS_CONTENT);
    }

    @Override
    ReferencedListIterator iterator() {
        return new ReferencedListIterator(descriptor(null), connectorMock);
    }

    @Override
    ReferencedListDescriptor descriptor(String context) {
        final NamedResource owner = NamedResource.create(RESOURCE.getURI());
        final Assertion hasList = Assertion.createObjectPropertyAssertion(URI.create(HAS_LIST.getURI()), false);
        final Assertion hasNext = Assertion.createObjectPropertyAssertion(URI.create(HAS_NEXT.getURI()), false);
        final Assertion hasContent = Assertion.createObjectPropertyAssertion(URI.create(HAS_CONTENT.getURI()), false);
        final ReferencedListDescriptor descriptor = new ReferencedListDescriptorImpl(owner, hasList, hasNext,
                hasContent);
        if (context != null) {
            descriptor.setContext(URI.create(context));
        }
        return descriptor;
    }

    @Override
    List<URI> generateList() {
        return testUtil.generateReferencedList();
    }

    @Test
    public void nextReturnsFirstListElement() {
        final List<URI> list = generateList();
        final AbstractListIterator iterator = iterator();
        assertTrue(iterator.hasNext());
        final Axiom<NamedResource> head = iterator.nextAxiom();
        assertNotNull(head);
        assertEquals(descriptor(null).getNodeContent(), head.getAssertion());
        assertEquals(NamedResource.create(list.get(0)), head.getValue().getValue());
    }

    @Test
    public void nextThrowsListProcessingExceptionWhenNodeIsLiteral() {
        generateList();
        when(connectorMock.find(RESOURCE, HAS_LIST, null)).thenReturn(
                Collections.singletonList(createStatement(RESOURCE, HAS_LIST, createTypedLiteral(117))));
        final ReferencedListIterator iterator = iterator();
        thrown.expect(ListProcessingException.class);
        thrown.expectMessage("Expected successor of node " + RESOURCE + " to be a named resource.");
        while (iterator.hasNext()) {
            iterator.nextAxiom();
        }
    }

    @Test
    public void nextThrowsIntegrityConstraintViolationExceptionWhenNodeHasMultipleContentValues() {
        final List<URI> list = generateList();
        final int index = Generator.randomInt(list.size());
        final Resource node = testUtil.getReferencedListNodes().get(index);
        when(connectorMock.find(node, HAS_CONTENT, null)).thenReturn(Arrays.asList(
                createStatement(node, HAS_CONTENT, createResource(Generator.generateUri().toString())),
                createStatement(node, HAS_CONTENT, createResource(list.get(index).toString()))
        ));
        final ReferencedListIterator iterator = iterator();
        thrown.expect(IntegrityConstraintViolatedException.class);
        thrown.expectMessage("Encountered multiple content values of list node " + node.getURI());
        while (iterator.hasNext()) {
            iterator.nextAxiom();
        }
    }

    @Test
    public void nextThrowsIntegrityConstraintViolationExceptionWhenNodeHasNoContent() {
        final List<URI> list = generateList();
        final int index = Generator.randomInt(list.size());
        final Resource node = testUtil.getReferencedListNodes().get(index);
        when(connectorMock.find(node, HAS_CONTENT, null)).thenReturn(Collections.emptyList());
        final ReferencedListIterator iterator = iterator();
        thrown.expect(IntegrityConstraintViolatedException.class);
        thrown.expectMessage("No content found for list node " + node.getURI());
        while (iterator.hasNext()) {
            iterator.nextAxiom();
        }
    }
}

package cz.cvut.kbss.ontodriver.jena.list;

import cz.cvut.kbss.ontodriver.descriptor.SimpleListDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.SimpleListDescriptorImpl;
import cz.cvut.kbss.ontodriver.descriptor.SimpleListValueDescriptor;
import cz.cvut.kbss.ontodriver.jena.connector.StorageConnector;
import cz.cvut.kbss.ontodriver.jena.environment.Generator;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.Statement;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static org.apache.jena.rdf.model.ResourceFactory.*;
import static org.junit.Assert.assertEquals;
import static org.mockito.Matchers.anyListOf;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

public class SimpleListHandlerTest {

    private static final NamedResource OWNER = NamedResource.create(Generator.generateUri());
    private static final Assertion HAS_LIST = Assertion.createObjectPropertyAssertion(Generator.generateUri(), false);
    private static final Assertion HAS_NEXT = Assertion.createObjectPropertyAssertion(Generator.generateUri(), false);
    private static final Resource OWNER_RESOURCE = createResource(OWNER.getIdentifier().toString());
    private static final Property HAS_LIST_PROPERTY = createProperty(HAS_LIST.getIdentifier().toString());
    private static final Property HAS_NEXT_PROPERTY = createProperty(HAS_NEXT.getIdentifier().toString());

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
                .loadList(new SimpleListDescriptorImpl(OWNER, HAS_LIST, HAS_NEXT));
        final List<URI> actual = result.stream().map(ax -> ax.getValue().getValue().getIdentifier())
                                       .collect(Collectors.toList());
        assertEquals(expected, actual);
    }

    private List<URI> generateList(String context) {
        if (context != null) {
            return ListTestUtil
                    .generateSimpleList(OWNER_RESOURCE, HAS_LIST_PROPERTY, HAS_NEXT_PROPERTY, connectorMock, context);
        } else {
            return ListTestUtil.generateSimpleList(OWNER_RESOURCE, HAS_LIST_PROPERTY, HAS_NEXT_PROPERTY, connectorMock);
        }
    }

    @Test
    public void loadListFromContextRetrievesAllListElements() {
        final URI context = Generator.generateUri();
        final List<URI> expected = generateList(context.toString());
        final SimpleListDescriptor descriptor = new SimpleListDescriptorImpl(OWNER, HAS_LIST, HAS_NEXT);
        descriptor.setContext(context);
        final List<Axiom<NamedResource>> result = handler.loadList(descriptor);
        final List<URI> actual = result.stream().map(ax -> ax.getValue().getValue().getIdentifier())
                                       .collect(Collectors.toList());
        assertEquals(expected, actual);
    }

    @Test
    public void persistListInsertsStatementsCorrespondingToList() {
        final List<URI> list = IntStream.range(0, 5).mapToObj(i -> Generator.generateUri())
                                        .collect(Collectors.toList());
        final SimpleListValueDescriptor descriptor = new SimpleListValueDescriptor(OWNER, HAS_LIST, HAS_NEXT);
        list.forEach(item -> descriptor.addValue(NamedResource.create(item)));
        handler.persistList(descriptor);
        final List<Statement> expected = getExpectedStatementsForPersist(list);
        verify(connectorMock).add(expected);
    }

    private List<Statement> getExpectedStatementsForPersist(List<URI> list) {
        final List<Statement> expected = new ArrayList<>(list.size());
        expected.add(createStatement(OWNER_RESOURCE,
                HAS_LIST_PROPERTY, createResource(list.get(0).toString())));
        final Property hasNext = createProperty(HAS_NEXT.getIdentifier().toString());
        for (int i = 0; i < list.size() - 1; i++) {
            expected.add(createStatement(createResource(list.get(i).toString()), hasNext,
                    createResource(list.get(i + 1).toString())));
        }
        return expected;
    }

    @Test
    public void persistListDoesNothingForEmptyDescriptor() {
        final SimpleListValueDescriptor descriptor = new SimpleListValueDescriptor(OWNER, HAS_LIST, HAS_NEXT);
        handler.persistList(descriptor);
        verify(connectorMock, never()).add(anyListOf(Statement.class));
    }

    @Test
    public void persistListWithContextInsertsStatementsCorrespondingToListIntoContext() {
        final List<URI> list = IntStream.range(0, 5).mapToObj(i -> Generator.generateUri())
                                        .collect(Collectors.toList());
        final SimpleListValueDescriptor descriptor = new SimpleListValueDescriptor(OWNER, HAS_LIST, HAS_NEXT);
        list.forEach(item -> descriptor.addValue(NamedResource.create(item)));
        final URI context = Generator.generateUri();
        descriptor.setContext(context);
        handler.persistList(descriptor);
        final List<Statement> expected = getExpectedStatementsForPersist(list);
        verify(connectorMock).add(expected, context.toString());
    }

    @Test
    public void updateListAddsNodeToEnd() {
        final List<URI> list = generateList(null);
        final URI newItem = Generator.generateUri();
        final SimpleListValueDescriptor descriptor = new SimpleListValueDescriptor(OWNER, HAS_LIST, HAS_NEXT);
        list.forEach(item -> descriptor.addValue(NamedResource.create(item)));
        descriptor.addValue(NamedResource.create(newItem));
        handler.updateList(descriptor);
        final Statement expectedAdd = createStatement(createResource(list.get(list.size() - 1).toString()),
                createProperty(HAS_NEXT.getIdentifier().toString()), createResource(newItem.toString()));
        verify(connectorMock).add(Collections.singletonList(expectedAdd));
    }

    @Test
    public void updateListRemovesRemainingExistingNodes() {
        final List<URI> list = generateList(null);
        final SimpleListValueDescriptor descriptor = new SimpleListValueDescriptor(OWNER, HAS_LIST, HAS_NEXT);
        final List<URI> update = list.subList(0, list.size() / 2);
        update.forEach(item -> descriptor.addValue(NamedResource.create(item)));
        handler.updateList(descriptor);
        final Property hasNext = createProperty(HAS_NEXT.getIdentifier().toString());
        for (int i = update.size() - 1; i < list.size() - 1; i++) {
            verify(connectorMock).remove(createResource(list.get(i).toString()), hasNext,
                    createResource(list.get(i + 1).toString()));
        }
    }

    @Test
    public void updateReplacesModifiedNode() {
        final List<URI> list = generateList(null);
        final SimpleListValueDescriptor descriptor = new SimpleListValueDescriptor(OWNER, HAS_LIST, HAS_NEXT);
        final List<URI> update = new ArrayList<>(list);
        final URI replace = Generator.generateUri();
        final int index = Generator.randomInt(list.size() - 1);
        update.set(index, replace);
        update.forEach(item -> descriptor.addValue(NamedResource.create(item)));
        handler.updateList(descriptor);
        final Resource previous = createResource(
                index > 0 ? list.get(index - 1).toString() : OWNER.getIdentifier().toString());
        final Resource removed = createResource(list.get(index).toString());
        final Resource next = createResource(list.get(index + 1).toString());
        final Property previousLink = index == 0 ? HAS_LIST_PROPERTY : HAS_NEXT_PROPERTY;
        verify(connectorMock).remove(previous, previousLink, removed);
        verify(connectorMock).remove(removed, HAS_NEXT_PROPERTY, next);
        final List<Statement> expectedAdded = new ArrayList<>(2);
        expectedAdded.add(createStatement(previous, previousLink, createResource(replace.toString())));
        expectedAdded.add(createStatement(createResource(replace.toString()),
                HAS_NEXT_PROPERTY, next));
        verify(connectorMock).add(expectedAdded);
    }

    @Test
    public void updateClearsOriginalListWhenUpdateIsEmpty() {
        final List<URI> list = generateList(null);
        final SimpleListValueDescriptor descriptor = new SimpleListValueDescriptor(OWNER, HAS_LIST, HAS_NEXT);
        handler.updateList(descriptor);
        for (int i = 0; i < list.size(); i++) {
            if (i == 0) {
                verify(connectorMock).remove(OWNER_RESOURCE,
                        HAS_LIST_PROPERTY, createResource(list.get(i).toString()));
            } else {
                verify(connectorMock).remove(createResource(list.get(i - 1).toString()),
                        HAS_NEXT_PROPERTY, createResource(list.get(i).toString()));
            }
        }
    }

    @Test
    public void updatePersistsListWhenOriginalWasEmpty() {
        final List<URI> update = IntStream.range(0, 5).mapToObj(i -> Generator.generateUri())
                                          .collect(Collectors.toList());
        final SimpleListValueDescriptor descriptor = new SimpleListValueDescriptor(OWNER, HAS_LIST, HAS_NEXT);
        update.forEach(v -> descriptor.addValue(NamedResource.create(v)));
        handler.updateList(descriptor);
        final List<Statement> expectedAdded = new ArrayList<>(update.size());
        expectedAdded.add(createStatement(OWNER_RESOURCE, HAS_LIST_PROPERTY, createResource(update.get(0).toString())));
        for (int i = 0; i < update.size() - 1; i++) {
            expectedAdded.add(createStatement(createResource(update.get(i).toString()), HAS_NEXT_PROPERTY,
                    createResource(update.get(i + 1).toString())));
        }
        verify(connectorMock).add(expectedAdded);
    }

    @Test
    public void updateListWorksInContext() {
        final URI context = Generator.generateUri();
        final List<URI> list = generateList(context.toString());
        final SimpleListValueDescriptor descriptor = new SimpleListValueDescriptor(OWNER, HAS_LIST, HAS_NEXT);
        descriptor.setContext(context);
        final URI firstReplaced = Generator.generateUri();
        descriptor.addValue(NamedResource.create(firstReplaced));
        list.subList(1, list.size()).forEach(item -> descriptor.addValue(NamedResource.create(item)));
        final URI added = Generator.generateUri();
        descriptor.addValue(NamedResource.create(added));
        handler.updateList(descriptor);
        verify(connectorMock)
                .remove(OWNER_RESOURCE, HAS_LIST_PROPERTY, createResource(list.get(0).toString()), context.toString());
        verify(connectorMock).remove(createResource(list.get(0).toString()), HAS_NEXT_PROPERTY,
                createResource(list.get(1).toString()), context.toString());
        verify(connectorMock).add(Arrays.asList(
                createStatement(OWNER_RESOURCE, HAS_LIST_PROPERTY, createResource(firstReplaced.toString())),
                createStatement(createResource(firstReplaced.toString()), HAS_NEXT_PROPERTY,
                        createResource(list.get(1).toString()))), context.toString());
        verify(connectorMock).add(Collections
                .singletonList(createStatement(createResource(list.get(list.size() - 1).toString()), HAS_NEXT_PROPERTY,
                        createProperty(added.toString()))), context.toString());
    }
}
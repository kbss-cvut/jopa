package cz.cvut.kbss.ontodriver.jena.list;

import cz.cvut.kbss.ontodriver.descriptor.*;
import cz.cvut.kbss.ontodriver.jena.JenaAdapter;
import cz.cvut.kbss.ontodriver.jena.environment.Generator;
import cz.cvut.kbss.ontodriver.jena.util.Procedure;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InOrder;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class JenaListsTest {

    private static final NamedResource RESOURCE = NamedResource.create(Generator.generateUri());
    private static final Assertion HAS_LIST = Assertion.createObjectPropertyAssertion(Generator.generateUri(), false);
    private static final Assertion HAS_NEXT = Assertion.createObjectPropertyAssertion(Generator.generateUri(), false);
    private static final Assertion HAS_CONTENT = Assertion
            .createObjectPropertyAssertion(Generator.generateUri(), false);

    @Mock
    private Procedure beforeMock;

    @Mock
    private Procedure afterMock;

    @Mock
    private SimpleListHandler simpleListHandlerMock;

    @Mock
    private ReferencedListHandler referencedListHandlerMock;

    private JenaLists lists;

    @Before
    public void setUp() {
        MockitoAnnotations.initMocks(this);
        final JenaAdapter adapterMock = mock(JenaAdapter.class);
        when(adapterMock.simpleListHandler()).thenReturn(simpleListHandlerMock);
        when(adapterMock.referencedListHandler()).thenReturn(referencedListHandlerMock);
        this.lists = new JenaLists(adapterMock, beforeMock, afterMock);
    }

    @Test
    public void loadSimpleListInvokesBeforeCallbackBeforeSimpleListHandler() throws Exception {
        final SimpleListDescriptor descriptor = new SimpleListDescriptorImpl(RESOURCE, HAS_LIST, HAS_NEXT);
        lists.loadSimpleList(descriptor);
        final InOrder inOrder = Mockito.inOrder(beforeMock, simpleListHandlerMock);
        inOrder.verify(beforeMock).execute();
        inOrder.verify(simpleListHandlerMock).loadList(descriptor);
    }

    @Test
    public void persistSimpleListInvokesBeforeCallbackBeforeSimpleListHandler() throws Exception {
        final SimpleListValueDescriptor descriptor = new SimpleListValueDescriptor(RESOURCE, HAS_LIST, HAS_NEXT);
        lists.persistSimpleList(descriptor);
        final InOrder inOrder = Mockito.inOrder(beforeMock, simpleListHandlerMock);
        inOrder.verify(beforeMock).execute();
        inOrder.verify(simpleListHandlerMock).persistList(descriptor);
    }

    @Test
    public void persistSimpleListInvokesAfterCallbackAfterSimpleListHandler() throws Exception {
        final SimpleListValueDescriptor descriptor = new SimpleListValueDescriptor(RESOURCE, HAS_LIST, HAS_NEXT);
        lists.persistSimpleList(descriptor);
        final InOrder inOrder = Mockito.inOrder(simpleListHandlerMock, afterMock);
        inOrder.verify(simpleListHandlerMock).persistList(descriptor);
        inOrder.verify(afterMock).execute();
    }

    @Test
    public void updateSimpleListInvokesBeforeCallbackBeforeSimpleListHandler() throws Exception {
        final SimpleListValueDescriptor descriptor = new SimpleListValueDescriptor(RESOURCE, HAS_LIST, HAS_NEXT);
        lists.updateSimpleList(descriptor);
        final InOrder inOrder = Mockito.inOrder(beforeMock, simpleListHandlerMock);
        inOrder.verify(beforeMock).execute();
        inOrder.verify(simpleListHandlerMock).updateList(descriptor);
    }

    @Test
    public void updateSimpleListInvokesAfterCallbackAfterSimpleListHandler() throws Exception {
        final SimpleListValueDescriptor descriptor = new SimpleListValueDescriptor(RESOURCE, HAS_LIST, HAS_NEXT);
        lists.updateSimpleList(descriptor);
        final InOrder inOrder = Mockito.inOrder(simpleListHandlerMock, afterMock);
        inOrder.verify(simpleListHandlerMock).updateList(descriptor);
        inOrder.verify(afterMock).execute();
    }

    @Test
    public void loadReferencedListExecutesBeforeCallbackBeforeReferencedListHandler() throws Exception {
        final ReferencedListDescriptor descriptor = new ReferencedListDescriptorImpl(RESOURCE, HAS_LIST, HAS_NEXT,
                HAS_CONTENT);
        lists.loadReferencedList(descriptor);
        final InOrder inOrder = Mockito.inOrder(beforeMock, referencedListHandlerMock);
        inOrder.verify(beforeMock).execute();
        inOrder.verify(referencedListHandlerMock).loadList(descriptor);
    }

    @Test
    public void persistReferencedListExecutesBeforeCallbackBeforeReferencedListHandler() throws Exception {
        final ReferencedListValueDescriptor descriptor = new ReferencedListValueDescriptor(RESOURCE, HAS_LIST, HAS_NEXT,
                HAS_CONTENT);
        lists.persistReferencedList(descriptor);
        final InOrder inOrder = Mockito.inOrder(beforeMock, referencedListHandlerMock);
        inOrder.verify(beforeMock).execute();
        inOrder.verify(referencedListHandlerMock).persistList(descriptor);
    }

    @Test
    public void persistReferencedListExecutesAfterCallbackAfterReferencedListHandler() throws Exception {
        final ReferencedListValueDescriptor descriptor = new ReferencedListValueDescriptor(RESOURCE, HAS_LIST, HAS_NEXT,
                HAS_CONTENT);
        lists.persistReferencedList(descriptor);
        final InOrder inOrder = Mockito.inOrder(referencedListHandlerMock, afterMock);
        inOrder.verify(referencedListHandlerMock).persistList(descriptor);
        inOrder.verify(afterMock).execute();
    }

    @Test
    public void updateReferencedListExecutesBeforeCallbackBeforeReferencedListHandler() throws Exception {
        final ReferencedListValueDescriptor descriptor = new ReferencedListValueDescriptor(RESOURCE, HAS_LIST, HAS_NEXT,
                HAS_CONTENT);
        lists.updateReferencedList(descriptor);
        final InOrder inOrder = Mockito.inOrder(beforeMock, referencedListHandlerMock);
        inOrder.verify(beforeMock).execute();
        inOrder.verify(referencedListHandlerMock).updateList(descriptor);
    }

    @Test
    public void updateReferencedListExecutesAfterCallbackAfterReferencedListHandler() throws Exception {
        final ReferencedListValueDescriptor descriptor = new ReferencedListValueDescriptor(RESOURCE, HAS_LIST, HAS_NEXT,
                HAS_CONTENT);
        lists.updateReferencedList(descriptor);
        final InOrder inOrder = Mockito.inOrder(referencedListHandlerMock, afterMock);
        inOrder.verify(referencedListHandlerMock).updateList(descriptor);
        inOrder.verify(afterMock).execute();
    }
}
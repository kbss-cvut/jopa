package cz.cvut.kbss.ontodriver.owlapi.list;

import cz.cvut.kbss.ontodriver.owlapi.OwlapiAdapter;
import cz.cvut.kbss.ontodriver.owlapi.OwlapiConnection;
import cz.cvut.kbss.ontodriver_new.descriptors.*;
import cz.cvut.kbss.ontodriver_new.model.NamedResource;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import static cz.cvut.kbss.ontodriver.owlapi.list.ListHandlerTestBase.*;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class OwlapiListsTest {


    @Mock
    private OwlapiAdapter adapterMock;
    @Mock
    private OwlapiConnection connectionMock;
    @Mock
    private SimpleListHandler simpleListHandlerMock;
    @Mock
    private ReferencedListHandler refListHandlerMock;

    private OwlapiLists lists;

    @Before
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        when(adapterMock.getSimpleListHandler()).thenReturn(simpleListHandlerMock);
        when(adapterMock.getReferencedListHandler()).thenReturn(refListHandlerMock);
        this.lists = new OwlapiLists(connectionMock, adapterMock);
    }

    @Test
    public void testLoadSimpleList() throws Exception {
        final SimpleListDescriptor descriptor = new SimpleListDescriptorImpl(SUBJECT, HAS_LIST, HAS_NEXT);
        lists.loadSimpleList(descriptor);
        verify(adapterMock).getSimpleListHandler();
        verify(simpleListHandlerMock).loadList(descriptor);
    }

    @Test
    public void testPersistSimpleList() throws Exception {
        final SimpleListValueDescriptor descriptor = new SimpleListValueDescriptor(SUBJECT, HAS_LIST, HAS_NEXT);
        descriptor.addValue(NamedResource.create("http://test"));
        lists.persistSimpleList(descriptor);
        verify(simpleListHandlerMock).persistList(descriptor);
        verify(connectionMock).commitIfAuto();
    }

    @Test
    public void testUpdateSimpleList() throws Exception {
        final SimpleListValueDescriptor descriptor = new SimpleListValueDescriptor(SUBJECT, HAS_LIST, HAS_NEXT);
        descriptor.addValue(NamedResource.create("http://test"));
        lists.updateSimpleList(descriptor);
        verify(simpleListHandlerMock).updateList(descriptor);
        verify(connectionMock).commitIfAuto();
    }

    @Test
    public void testLoadReferencedList() throws Exception {
        final ReferencedListDescriptor descriptor = new ReferencedListDescriptorImpl(SUBJECT, HAS_LIST, HAS_NEXT,
                HAS_CONTENT);
        lists.loadReferencedList(descriptor);
        verify(refListHandlerMock).loadList(descriptor);
    }

    @Test
    public void testPersistReferencedList() throws Exception {
        final ReferencedListValueDescriptor descriptor = new ReferencedListValueDescriptor(SUBJECT, HAS_LIST, HAS_NEXT,
                HAS_CONTENT);
        descriptor.addValue(NamedResource.create("http://test"));
        lists.persistReferencedList(descriptor);
        verify(refListHandlerMock).persistList(descriptor);
        verify(connectionMock).commitIfAuto();
    }
}
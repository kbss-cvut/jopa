package cz.cvut.kbss.ontodriver.sesame;

import cz.cvut.kbss.ontodriver.descriptor.ReferencedListDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.ReferencedListValueDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.SimpleListDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.SimpleListValueDescriptor;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import static org.mockito.Mockito.*;

public class SesameListsTest {

	@Mock
	private SesameConnection connectionMock;

	@Mock
	private SesameAdapter adapterMock;

	@Mock
	private SimpleListHandler simpleListHandlerMock;

	@Mock
	private ReferencedListHandler referencedListHandlerMock;

	private SesameLists lists;

	@Before
	public void setUp() throws Exception {
		MockitoAnnotations.initMocks(this);
		when(adapterMock.getSimpleListHandler()).thenReturn(simpleListHandlerMock);
		when(adapterMock.getReferencedListHandler()).thenReturn(referencedListHandlerMock);

		this.lists = new SesameLists(connectionMock, adapterMock);
	}

	@Test
	public void testLoadSimpleList() throws Exception {
		final SimpleListDescriptor descriptor = mock(SimpleListDescriptor.class);

		lists.loadSimpleList(descriptor);

		verify(connectionMock).ensureOpen();
		verify(adapterMock).getSimpleListHandler();
		verify(simpleListHandlerMock).loadList(descriptor);
	}

	@Test
	public void testPersistSimpleList() throws Exception {
		final SimpleListValueDescriptor descriptor = mock(SimpleListValueDescriptor.class);

		lists.persistSimpleList(descriptor);

		verify(connectionMock).ensureOpen();
		verify(adapterMock).getSimpleListHandler();
		verify(simpleListHandlerMock).persistList(descriptor);
		verify(connectionMock).commitIfAuto();
	}

	@Test
	public void testUpdateSimpleList() throws Exception {
		final SimpleListValueDescriptor descriptor = mock(SimpleListValueDescriptor.class);

		lists.updateSimpleList(descriptor);

		verify(connectionMock).ensureOpen();
		verify(adapterMock).getSimpleListHandler();
		verify(simpleListHandlerMock).updateList(descriptor);
		verify(connectionMock).commitIfAuto();
	}

	@Test
	public void testLoadReferencedList() throws Exception {
		final ReferencedListDescriptor descriptor = mock(ReferencedListDescriptor.class);

		lists.loadReferencedList(descriptor);

		verify(connectionMock).ensureOpen();
		verify(adapterMock).getReferencedListHandler();
		verify(referencedListHandlerMock).loadList(descriptor);
	}

	@Test
	public void testPersistReferencedList() throws Exception {
		final ReferencedListValueDescriptor descriptor = mock(ReferencedListValueDescriptor.class);

		lists.persistReferencedList(descriptor);

		verify(connectionMock).ensureOpen();
		verify(adapterMock).getReferencedListHandler();
		verify(referencedListHandlerMock).persistList(descriptor);
		verify(connectionMock).commitIfAuto();
	}

	@Test
	public void testUpdateReferencedList() throws Exception {
		final ReferencedListValueDescriptor descriptor = mock(ReferencedListValueDescriptor.class);

		lists.updateReferencedList(descriptor);

		verify(connectionMock).ensureOpen();
		verify(adapterMock).getReferencedListHandler();
		verify(referencedListHandlerMock).updateList(descriptor);
		verify(connectionMock).commitIfAuto();
	}
}

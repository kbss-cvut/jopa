package cz.cvut.kbss.ontodriver.sesame;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import cz.cvut.kbss.ontodriver_new.descriptors.SimpleListDescriptor;
import cz.cvut.kbss.ontodriver_new.descriptors.SimpleListValueDescriptor;

public class SesameListsTest {

	@Mock
	private SesameConnection connectionMock;

	@Mock
	private SesameAdapter adapterMock;

	@Mock
	private SimpleListHandler simpleListHandlerMock;

	private SesameLists lists;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
	}

	@Before
	public void setUp() throws Exception {
		MockitoAnnotations.initMocks(this);
		when(adapterMock.getSimpleListHandler()).thenReturn(simpleListHandlerMock);

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
}

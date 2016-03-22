/**
 * Copyright (C) 2016 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
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
	private Procedure beforeMock;

	@Mock
    private Procedure afterMock;

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

		this.lists = new SesameLists(adapterMock, beforeMock, afterMock);
	}

	@Test
	public void testLoadSimpleList() throws Exception {
		final SimpleListDescriptor descriptor = mock(SimpleListDescriptor.class);

		lists.loadSimpleList(descriptor);

		verify(beforeMock).execute();
		verify(adapterMock).getSimpleListHandler();
		verify(simpleListHandlerMock).loadList(descriptor);
	}

	@Test
	public void testPersistSimpleList() throws Exception {
		final SimpleListValueDescriptor descriptor = mock(SimpleListValueDescriptor.class);

		lists.persistSimpleList(descriptor);

		verify(beforeMock).execute();
		verify(adapterMock).getSimpleListHandler();
		verify(simpleListHandlerMock).persistList(descriptor);
		verify(afterMock).execute();
	}

	@Test
	public void testUpdateSimpleList() throws Exception {
		final SimpleListValueDescriptor descriptor = mock(SimpleListValueDescriptor.class);

		lists.updateSimpleList(descriptor);

		verify(beforeMock).execute();
		verify(adapterMock).getSimpleListHandler();
		verify(simpleListHandlerMock).updateList(descriptor);
		verify(afterMock).execute();
	}

	@Test
	public void testLoadReferencedList() throws Exception {
		final ReferencedListDescriptor descriptor = mock(ReferencedListDescriptor.class);

		lists.loadReferencedList(descriptor);

		verify(beforeMock).execute();
		verify(adapterMock).getReferencedListHandler();
		verify(referencedListHandlerMock).loadList(descriptor);
	}

	@Test
	public void testPersistReferencedList() throws Exception {
		final ReferencedListValueDescriptor descriptor = mock(ReferencedListValueDescriptor.class);

		lists.persistReferencedList(descriptor);

		verify(beforeMock).execute();
		verify(adapterMock).getReferencedListHandler();
		verify(referencedListHandlerMock).persistList(descriptor);
		verify(afterMock).execute();
	}

	@Test
	public void testUpdateReferencedList() throws Exception {
		final ReferencedListValueDescriptor descriptor = mock(ReferencedListValueDescriptor.class);

		lists.updateReferencedList(descriptor);

		verify(beforeMock).execute();
		verify(adapterMock).getReferencedListHandler();
		verify(referencedListHandlerMock).updateList(descriptor);
		verify(afterMock).execute();
	}
}

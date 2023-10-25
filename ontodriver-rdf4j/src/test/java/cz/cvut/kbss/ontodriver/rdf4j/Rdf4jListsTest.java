/*
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.ontodriver.rdf4j;

import cz.cvut.kbss.ontodriver.descriptor.ReferencedListDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.ReferencedListValueDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.SimpleListDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.SimpleListValueDescriptor;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
public class Rdf4jListsTest {

    @Mock
    private Procedure beforeMock;

    @Mock
    private Procedure afterMock;

    @Mock
    private Rdf4jAdapter adapterMock;

    @Mock
    private SimpleListHandler simpleListHandlerMock;

    @Mock
    private ReferencedListHandler referencedListHandlerMock;

    private Rdf4jLists lists;

    @BeforeEach
    public void setUp() {
        this.lists = new Rdf4jLists(adapterMock, beforeMock, afterMock);
    }

    @Test
    public void testLoadSimpleList() throws Exception {
        when(adapterMock.getSimpleListHandler()).thenReturn(simpleListHandlerMock);
        final SimpleListDescriptor descriptor = mock(SimpleListDescriptor.class);

        lists.loadSimpleList(descriptor);

        verify(beforeMock).execute();
        verify(adapterMock).getSimpleListHandler();
        verify(simpleListHandlerMock).loadList(descriptor);
    }

    @Test
    public void testPersistSimpleList() throws Exception {
        when(adapterMock.getSimpleListHandler()).thenReturn(simpleListHandlerMock);
        final SimpleListValueDescriptor descriptor = mock(SimpleListValueDescriptor.class);

        lists.persistSimpleList(descriptor);

        verify(beforeMock).execute();
        verify(adapterMock).getSimpleListHandler();
        verify(simpleListHandlerMock).persistList(descriptor);
        verify(afterMock).execute();
    }

    @Test
    public void testUpdateSimpleList() throws Exception {
        when(adapterMock.getSimpleListHandler()).thenReturn(simpleListHandlerMock);
        final SimpleListValueDescriptor descriptor = mock(SimpleListValueDescriptor.class);

        lists.updateSimpleList(descriptor);

        verify(beforeMock).execute();
        verify(adapterMock).getSimpleListHandler();
        verify(simpleListHandlerMock).updateList(descriptor);
        verify(afterMock).execute();
    }

    @Test
    public void testLoadReferencedList() throws Exception {
        when(adapterMock.getReferencedListHandler()).thenReturn(referencedListHandlerMock);
        final ReferencedListDescriptor descriptor = mock(ReferencedListDescriptor.class);

        lists.loadReferencedList(descriptor);

        verify(beforeMock).execute();
        verify(adapterMock).getReferencedListHandler();
        verify(referencedListHandlerMock).loadList(descriptor);
    }

    @Test
    public void testPersistReferencedList() throws Exception {
        when(adapterMock.getReferencedListHandler()).thenReturn(referencedListHandlerMock);
        final ReferencedListValueDescriptor descriptor = mock(ReferencedListValueDescriptor.class);

        lists.persistReferencedList(descriptor);

        verify(beforeMock).execute();
        verify(adapterMock).getReferencedListHandler();
        verify(referencedListHandlerMock).persistList(descriptor);
        verify(afterMock).execute();
    }

    @Test
    public void testUpdateReferencedList() throws Exception {
        when(adapterMock.getReferencedListHandler()).thenReturn(referencedListHandlerMock);
        final ReferencedListValueDescriptor descriptor = mock(ReferencedListValueDescriptor.class);

        lists.updateReferencedList(descriptor);

        verify(beforeMock).execute();
        verify(adapterMock).getReferencedListHandler();
        verify(referencedListHandlerMock).updateList(descriptor);
        verify(afterMock).execute();
    }
}

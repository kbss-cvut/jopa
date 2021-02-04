/**
 * Copyright (C) 2020 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.owlapi.list;

import cz.cvut.kbss.ontodriver.descriptor.*;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.owlapi.OwlapiAdapter;
import cz.cvut.kbss.ontodriver.owlapi.util.Procedure;
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
    private Procedure afterMock;
    @Mock
    private SimpleListHandler simpleListHandlerMock;
    @Mock
    private ReferencedListHandler refListHandlerMock;

    private OwlapiLists lists;

    @Before
    public void setUp() throws Exception {
        MockitoAnnotations.openMocks(this);
        when(adapterMock.getSimpleListHandler()).thenReturn(simpleListHandlerMock);
        when(adapterMock.getReferencedListHandler()).thenReturn(refListHandlerMock);
        this.lists = new OwlapiLists(adapterMock, () -> {}, afterMock);
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
        verify(afterMock).execute();
    }

    @Test
    public void testUpdateSimpleList() throws Exception {
        final SimpleListValueDescriptor descriptor = new SimpleListValueDescriptor(SUBJECT, HAS_LIST, HAS_NEXT);
        descriptor.addValue(NamedResource.create("http://test"));
        lists.updateSimpleList(descriptor);
        verify(simpleListHandlerMock).updateList(descriptor);
        verify(afterMock).execute();
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
        verify(afterMock).execute();
    }

    @Test
    public void testUpdateReferencedList() throws Exception {
        final ReferencedListValueDescriptor descriptor = new ReferencedListValueDescriptor(SUBJECT, HAS_LIST, HAS_NEXT,
                HAS_CONTENT);
        descriptor.addValue(NamedResource.create("http://test"));
        lists.updateReferencedList(descriptor);
        verify(refListHandlerMock).updateList(descriptor);
        verify(afterMock).execute();
    }
}
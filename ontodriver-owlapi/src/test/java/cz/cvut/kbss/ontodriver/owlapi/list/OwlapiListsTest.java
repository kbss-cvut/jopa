/*
 * JOPA
 * Copyright (C) 2025 Czech Technical University in Prague
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */
package cz.cvut.kbss.ontodriver.owlapi.list;

import cz.cvut.kbss.ontodriver.descriptor.ReferencedListDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.ReferencedListDescriptorImpl;
import cz.cvut.kbss.ontodriver.descriptor.ReferencedListValueDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.SimpleListDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.SimpleListDescriptorImpl;
import cz.cvut.kbss.ontodriver.descriptor.SimpleListValueDescriptor;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.owlapi.OwlapiAdapter;
import cz.cvut.kbss.ontodriver.owlapi.util.Procedure;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
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

    @BeforeEach
    public void setUp() {
        this.lists = new OwlapiLists(adapterMock, () -> {
        }, afterMock);
    }

    @Test
    public void testLoadSimpleList() throws Exception {
        when(adapterMock.getSimpleListHandler()).thenReturn(simpleListHandlerMock);
        final SimpleListDescriptor descriptor = new SimpleListDescriptorImpl(ListTestHelper.SUBJECT, ListTestHelper.HAS_LIST, ListTestHelper.HAS_NEXT);
        lists.loadSimpleList(descriptor);
        verify(adapterMock).getSimpleListHandler();
        verify(simpleListHandlerMock).loadList(descriptor);
    }

    @Test
    public void testPersistSimpleList() throws Exception {
        when(adapterMock.getSimpleListHandler()).thenReturn(simpleListHandlerMock);
        final SimpleListValueDescriptor descriptor = new SimpleListValueDescriptor(ListTestHelper.SUBJECT, ListTestHelper.HAS_LIST, ListTestHelper.HAS_NEXT);
        descriptor.addValue(NamedResource.create("http://test"));
        lists.persistSimpleList(descriptor);
        verify(simpleListHandlerMock).persistList(descriptor);
        verify(afterMock).execute();
    }

    @Test
    public void testUpdateSimpleList() throws Exception {
        when(adapterMock.getSimpleListHandler()).thenReturn(simpleListHandlerMock);
        final SimpleListValueDescriptor descriptor = new SimpleListValueDescriptor(ListTestHelper.SUBJECT, ListTestHelper.HAS_LIST, ListTestHelper.HAS_NEXT);
        descriptor.addValue(NamedResource.create("http://test"));
        lists.updateSimpleList(descriptor);
        verify(simpleListHandlerMock).updateList(descriptor);
        verify(afterMock).execute();
    }

    @Test
    public void testLoadReferencedList() throws Exception {
        when(adapterMock.getReferencedListHandler()).thenReturn(refListHandlerMock);
        final ReferencedListDescriptor descriptor = new ReferencedListDescriptorImpl(ListTestHelper.SUBJECT, ListTestHelper.HAS_LIST, ListTestHelper.HAS_NEXT,
                ListTestHelper.HAS_CONTENT);
        lists.loadReferencedList(descriptor);
        verify(refListHandlerMock).loadList(descriptor);
    }

    @Test
    public void testPersistReferencedList() throws Exception {
        when(adapterMock.getReferencedListHandler()).thenReturn(refListHandlerMock);
        final ReferencedListValueDescriptor<NamedResource> descriptor = new ReferencedListValueDescriptor<>(ListTestHelper.SUBJECT, ListTestHelper.HAS_LIST, ListTestHelper.HAS_NEXT,
                ListTestHelper.HAS_CONTENT);
        descriptor.addValue(NamedResource.create("http://test"));
        lists.persistReferencedList(descriptor);
        verify(refListHandlerMock).persistList(descriptor);
        verify(afterMock).execute();
    }

    @Test
    public void testUpdateReferencedList() throws Exception {
        when(adapterMock.getReferencedListHandler()).thenReturn(refListHandlerMock);
        final ReferencedListValueDescriptor<NamedResource> descriptor = new ReferencedListValueDescriptor<>(ListTestHelper.SUBJECT, ListTestHelper.HAS_LIST, ListTestHelper.HAS_NEXT,
                ListTestHelper.HAS_CONTENT);
        descriptor.addValue(NamedResource.create("http://test"));
        lists.updateReferencedList(descriptor);
        verify(refListHandlerMock).updateList(descriptor);
        verify(afterMock).execute();
    }
}

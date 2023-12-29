/*
 * JOPA
 * Copyright (C) 2023 Czech Technical University in Prague
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

import cz.cvut.kbss.ontodriver.descriptor.*;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.owlapi.OwlapiAdapter;
import cz.cvut.kbss.ontodriver.owlapi.util.Procedure;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

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

    @BeforeEach
    public void setUp() {
        MockitoAnnotations.openMocks(this);
        when(adapterMock.getSimpleListHandler()).thenReturn(simpleListHandlerMock);
        when(adapterMock.getReferencedListHandler()).thenReturn(refListHandlerMock);
        this.lists = new OwlapiLists(adapterMock, () -> {
        }, afterMock);
    }

    @Test
    public void testLoadSimpleList() throws Exception {
        final SimpleListDescriptor descriptor = new SimpleListDescriptorImpl(ListTestHelper.SUBJECT, ListTestHelper.HAS_LIST, ListTestHelper.HAS_NEXT);
        lists.loadSimpleList(descriptor);
        verify(adapterMock).getSimpleListHandler();
        verify(simpleListHandlerMock).loadList(descriptor);
    }

    @Test
    public void testPersistSimpleList() throws Exception {
        final SimpleListValueDescriptor descriptor = new SimpleListValueDescriptor(ListTestHelper.SUBJECT, ListTestHelper.HAS_LIST, ListTestHelper.HAS_NEXT);
        descriptor.addValue(NamedResource.create("http://test"));
        lists.persistSimpleList(descriptor);
        verify(simpleListHandlerMock).persistList(descriptor);
        verify(afterMock).execute();
    }

    @Test
    public void testUpdateSimpleList() throws Exception {
        final SimpleListValueDescriptor descriptor = new SimpleListValueDescriptor(ListTestHelper.SUBJECT, ListTestHelper.HAS_LIST, ListTestHelper.HAS_NEXT);
        descriptor.addValue(NamedResource.create("http://test"));
        lists.updateSimpleList(descriptor);
        verify(simpleListHandlerMock).updateList(descriptor);
        verify(afterMock).execute();
    }

    @Test
    public void testLoadReferencedList() throws Exception {
        final ReferencedListDescriptor descriptor = new ReferencedListDescriptorImpl(ListTestHelper.SUBJECT, ListTestHelper.HAS_LIST, ListTestHelper.HAS_NEXT,
                ListTestHelper.HAS_CONTENT);
        lists.loadReferencedList(descriptor);
        verify(refListHandlerMock).loadList(descriptor);
    }

    @Test
    public void testPersistReferencedList() throws Exception {
        final ReferencedListValueDescriptor<NamedResource> descriptor = new ReferencedListValueDescriptor<>(ListTestHelper.SUBJECT, ListTestHelper.HAS_LIST, ListTestHelper.HAS_NEXT,
                ListTestHelper.HAS_CONTENT);
        descriptor.addValue(NamedResource.create("http://test"));
        lists.persistReferencedList(descriptor);
        verify(refListHandlerMock).persistList(descriptor);
        verify(afterMock).execute();
    }

    @Test
    public void testUpdateReferencedList() throws Exception {
        final ReferencedListValueDescriptor<NamedResource> descriptor = new ReferencedListValueDescriptor<NamedResource>(ListTestHelper.SUBJECT, ListTestHelper.HAS_LIST, ListTestHelper.HAS_NEXT,
                ListTestHelper.HAS_CONTENT);
        descriptor.addValue(NamedResource.create("http://test"));
        lists.updateReferencedList(descriptor);
        verify(refListHandlerMock).updateList(descriptor);
        verify(afterMock).execute();
    }
}

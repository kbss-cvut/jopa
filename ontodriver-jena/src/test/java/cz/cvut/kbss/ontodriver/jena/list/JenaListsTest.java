/*
 * JOPA
 * Copyright (C) 2024 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.jena.list;

import cz.cvut.kbss.ontodriver.descriptor.*;
import cz.cvut.kbss.ontodriver.jena.JenaAdapter;
import cz.cvut.kbss.ontodriver.jena.environment.Generator;
import cz.cvut.kbss.ontodriver.jena.util.Procedure;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InOrder;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
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

    @Mock
    private JenaAdapter adapterMock;

    private JenaLists lists;

    @BeforeEach
    public void setUp() {
        this.lists = new JenaLists(adapterMock, beforeMock, afterMock);
    }

    @Test
    public void loadSimpleListInvokesBeforeCallbackBeforeSimpleListHandler() throws Exception {
        when(adapterMock.simpleListHandler()).thenReturn(simpleListHandlerMock);
        final SimpleListDescriptor descriptor = new SimpleListDescriptorImpl(RESOURCE, HAS_LIST, HAS_NEXT);
        lists.loadSimpleList(descriptor);
        final InOrder inOrder = Mockito.inOrder(beforeMock, simpleListHandlerMock);
        inOrder.verify(beforeMock).execute();
        inOrder.verify(simpleListHandlerMock).loadList(descriptor);
    }

    @Test
    public void persistSimpleListInvokesBeforeCallbackBeforeSimpleListHandler() throws Exception {
        when(adapterMock.simpleListHandler()).thenReturn(simpleListHandlerMock);
        final SimpleListValueDescriptor descriptor = new SimpleListValueDescriptor(RESOURCE, HAS_LIST, HAS_NEXT);
        lists.persistSimpleList(descriptor);
        final InOrder inOrder = Mockito.inOrder(beforeMock, simpleListHandlerMock);
        inOrder.verify(beforeMock).execute();
        inOrder.verify(simpleListHandlerMock).persistList(descriptor);
    }

    @Test
    public void persistSimpleListInvokesAfterCallbackAfterSimpleListHandler() throws Exception {
        when(adapterMock.simpleListHandler()).thenReturn(simpleListHandlerMock);
        final SimpleListValueDescriptor descriptor = new SimpleListValueDescriptor(RESOURCE, HAS_LIST, HAS_NEXT);
        lists.persistSimpleList(descriptor);
        final InOrder inOrder = Mockito.inOrder(simpleListHandlerMock, afterMock);
        inOrder.verify(simpleListHandlerMock).persistList(descriptor);
        inOrder.verify(afterMock).execute();
    }

    @Test
    public void updateSimpleListInvokesBeforeCallbackBeforeSimpleListHandler() throws Exception {
        when(adapterMock.simpleListHandler()).thenReturn(simpleListHandlerMock);
        final SimpleListValueDescriptor descriptor = new SimpleListValueDescriptor(RESOURCE, HAS_LIST, HAS_NEXT);
        lists.updateSimpleList(descriptor);
        final InOrder inOrder = Mockito.inOrder(beforeMock, simpleListHandlerMock);
        inOrder.verify(beforeMock).execute();
        inOrder.verify(simpleListHandlerMock).updateList(descriptor);
    }

    @Test
    public void updateSimpleListInvokesAfterCallbackAfterSimpleListHandler() throws Exception {
        when(adapterMock.simpleListHandler()).thenReturn(simpleListHandlerMock);
        final SimpleListValueDescriptor descriptor = new SimpleListValueDescriptor(RESOURCE, HAS_LIST, HAS_NEXT);
        lists.updateSimpleList(descriptor);
        final InOrder inOrder = Mockito.inOrder(simpleListHandlerMock, afterMock);
        inOrder.verify(simpleListHandlerMock).updateList(descriptor);
        inOrder.verify(afterMock).execute();
    }

    @Test
    public void loadReferencedListExecutesBeforeCallbackBeforeReferencedListHandler() throws Exception {
        when(adapterMock.referencedListHandler()).thenReturn(referencedListHandlerMock);
        final ReferencedListDescriptor descriptor = new ReferencedListDescriptorImpl(RESOURCE, HAS_LIST, HAS_NEXT,
                HAS_CONTENT);
        lists.loadReferencedList(descriptor);
        final InOrder inOrder = Mockito.inOrder(beforeMock, referencedListHandlerMock);
        inOrder.verify(beforeMock).execute();
        inOrder.verify(referencedListHandlerMock).loadList(descriptor);
    }

    @Test
    public void persistReferencedListExecutesBeforeCallbackBeforeReferencedListHandler() throws Exception {
        when(adapterMock.referencedListHandler()).thenReturn(referencedListHandlerMock);
        final ReferencedListValueDescriptor descriptor = new ReferencedListValueDescriptor(RESOURCE, HAS_LIST, HAS_NEXT,
                HAS_CONTENT);
        lists.persistReferencedList(descriptor);
        final InOrder inOrder = Mockito.inOrder(beforeMock, referencedListHandlerMock);
        inOrder.verify(beforeMock).execute();
        inOrder.verify(referencedListHandlerMock).persistList(descriptor);
    }

    @Test
    public void persistReferencedListExecutesAfterCallbackAfterReferencedListHandler() throws Exception {
        when(adapterMock.referencedListHandler()).thenReturn(referencedListHandlerMock);
        final ReferencedListValueDescriptor descriptor = new ReferencedListValueDescriptor(RESOURCE, HAS_LIST, HAS_NEXT,
                HAS_CONTENT);
        lists.persistReferencedList(descriptor);
        final InOrder inOrder = Mockito.inOrder(referencedListHandlerMock, afterMock);
        inOrder.verify(referencedListHandlerMock).persistList(descriptor);
        inOrder.verify(afterMock).execute();
    }

    @Test
    public void updateReferencedListExecutesBeforeCallbackBeforeReferencedListHandler() throws Exception {
        when(adapterMock.referencedListHandler()).thenReturn(referencedListHandlerMock);
        final ReferencedListValueDescriptor descriptor = new ReferencedListValueDescriptor(RESOURCE, HAS_LIST, HAS_NEXT,
                HAS_CONTENT);
        lists.updateReferencedList(descriptor);
        final InOrder inOrder = Mockito.inOrder(beforeMock, referencedListHandlerMock);
        inOrder.verify(beforeMock).execute();
        inOrder.verify(referencedListHandlerMock).updateList(descriptor);
    }

    @Test
    public void updateReferencedListExecutesAfterCallbackAfterReferencedListHandler() throws Exception {
        when(adapterMock.referencedListHandler()).thenReturn(referencedListHandlerMock);
        final ReferencedListValueDescriptor descriptor = new ReferencedListValueDescriptor(RESOURCE, HAS_LIST, HAS_NEXT,
                HAS_CONTENT);
        lists.updateReferencedList(descriptor);
        final InOrder inOrder = Mockito.inOrder(referencedListHandlerMock, afterMock);
        inOrder.verify(referencedListHandlerMock).updateList(descriptor);
        inOrder.verify(afterMock).execute();
    }
}

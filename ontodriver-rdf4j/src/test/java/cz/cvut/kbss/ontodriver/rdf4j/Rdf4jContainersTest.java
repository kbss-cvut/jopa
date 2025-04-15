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
package cz.cvut.kbss.ontodriver.rdf4j;

import cz.cvut.kbss.ontodriver.descriptor.ContainerDescriptor;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.AxiomImpl;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;
import cz.cvut.kbss.ontodriver.rdf4j.container.ContainerHandler;
import cz.cvut.kbss.ontodriver.rdf4j.environment.Generator;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InOrder;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.List;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class Rdf4jContainersTest {

    @Mock
    private ContainerHandler containerHandler;

    @Mock
    private Procedure beforeCallback;

    private Rdf4jContainers sut;

    @BeforeEach
    void setUp() throws Exception {
        final Rdf4jAdapter adapter = mock(Rdf4jAdapter.class);
        when(adapter.getContainerHandler()).thenReturn(containerHandler);
        this.sut = new Rdf4jContainers(adapter, beforeCallback, mock(Procedure.class));
    }

    @Test
    void readContainerInvokesPreCallbackAndThenRetrievesContainerContentAndReturnsIt() throws Exception {
        final NamedResource subject = NamedResource.create(Generator.generateUri());
        final Assertion property = Assertion.createDataPropertyAssertion(Generator.generateUri(), false);
        final List<Axiom<?>> result = List.of(new AxiomImpl<>(subject, property, new Value<>(1)));
        when(containerHandler.loadContainer(any())).thenReturn(result);
        final ContainerDescriptor descriptor = ContainerDescriptor.seqDescriptor(subject, property);
        assertEquals(result, sut.readContainer(descriptor));
        final InOrder inOrder = Mockito.inOrder(containerHandler, beforeCallback);
        inOrder.verify(beforeCallback).execute();
        inOrder.verify(containerHandler).loadContainer(descriptor);
    }
}

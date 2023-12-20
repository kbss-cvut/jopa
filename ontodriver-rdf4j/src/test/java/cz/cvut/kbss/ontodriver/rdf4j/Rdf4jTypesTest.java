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
package cz.cvut.kbss.ontodriver.rdf4j;

import cz.cvut.kbss.ontodriver.model.*;
import cz.cvut.kbss.ontodriver.rdf4j.environment.Generator;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.net.URI;
import java.util.Collections;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
public class Rdf4jTypesTest {

    private static final NamedResource INDIVIDUAL = NamedResource.create(Generator.generateUri());

    @Mock
    private Rdf4jAdapter adapterMock;
    @Mock
    private TypesHandler handlerMock;

    private Rdf4jTypes types;

    @BeforeEach
    public void setUp() {
        this.types = new Rdf4jTypes(adapterMock, () -> {
        }, () -> {
        });
    }

    @Test
    public void testGetTypes() throws Exception {
        when(adapterMock.getTypesHandler()).thenReturn(handlerMock);
        when(handlerMock.getTypes(INDIVIDUAL, Collections.emptySet(), false)).
                thenReturn(Collections.singleton(
                        new AxiomImpl<>(INDIVIDUAL,
                                Assertion
                                        .createClassAssertion(
                                                false),
                                new Value<>(
                                        INDIVIDUAL
                                                .getIdentifier()))));
        final Set<Axiom<URI>> res = types.getTypes(INDIVIDUAL, Collections.emptySet(), false);
        assertEquals(1, res.size());
        assertEquals(INDIVIDUAL.getIdentifier(), res.iterator().next().getValue().getValue());
        verify(handlerMock).getTypes(INDIVIDUAL, Collections.emptySet(), false);
    }

    @Test
    public void testAddTypes() throws Exception {
        when(adapterMock.getTypesHandler()).thenReturn(handlerMock);
        final Set<URI> toAdd = Collections.singleton(INDIVIDUAL.getIdentifier());
        types.addTypes(INDIVIDUAL, null, toAdd);
        verify(handlerMock).addTypes(INDIVIDUAL, null, toAdd);
    }

    @Test
    public void testAddEmpty() throws Exception {
        final Set<URI> toAdd = Collections.emptySet();
        types.addTypes(INDIVIDUAL, null, toAdd);
        verify(adapterMock, never()).getTypesHandler();
    }

    @Test
    public void testRemoveTypes() throws Exception {
        when(adapterMock.getTypesHandler()).thenReturn(handlerMock);
        final Set<URI> toRemove = Collections.singleton(INDIVIDUAL.getIdentifier());
        types.removeTypes(INDIVIDUAL, null, toRemove);
        verify(handlerMock).removeTypes(INDIVIDUAL, null, toRemove);
    }

    @Test
    public void testRemoveEmpty() throws Exception {
        final Set<URI> toRemove = Collections.emptySet();
        types.removeTypes(INDIVIDUAL, null, toRemove);
        verify(adapterMock, never()).getTypesHandler();
    }
}

/*
 * JOPA
 * Copyright (C) 2026 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.owlapi;

import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;
import cz.cvut.kbss.ontodriver.owlapi.util.Procedure;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Map;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.anyBoolean;
import static org.mockito.Mockito.anyMap;
import static org.mockito.Mockito.eq;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
public class OwlapiPropertiesTest {

    private static final URI IDENTIFIER = URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#Individual");
    private static final NamedResource INDIVIDUAL = NamedResource.create(IDENTIFIER);

    @Mock
    private PropertiesHandler handlerMock;

    @Mock
    private OwlapiAdapter adapterMock;

    @Mock
    private Procedure beforeMock;
    @Mock
    private Procedure afterMock;

    private OwlapiProperties properties;

    @BeforeEach
    public void setUp() {
        this.properties = new OwlapiProperties(adapterMock, beforeMock, afterMock);
    }

    @Test
    public void getPropertiesChecksForConnectionActivity() throws Exception {
        when(adapterMock.getPropertiesHandler()).thenReturn(handlerMock);
        final Collection<Axiom<?>> props = new ArrayList<>();
        when(handlerMock.getProperties(eq(INDIVIDUAL), anyBoolean())).thenReturn(props);
        final Collection<Axiom<?>> result = properties.getProperties(INDIVIDUAL, null, true);
        assertSame(props, result);
        verify(beforeMock).execute();
    }

    @Test
    public void addPropertiesCommitsTransactionIfTurnedOn() throws Exception {
        when(adapterMock.getPropertiesHandler()).thenReturn(handlerMock);
        final Map<Assertion, Set<Value<?>>> props = Collections.singletonMap(
                Assertion.createDataPropertyAssertion(URI.create("http://assertion"), false),
                Collections.singleton(new Value<>("String")));
        properties.addProperties(INDIVIDUAL, null, props);
        verify(handlerMock).addProperties(INDIVIDUAL, props);
        verify(afterMock).execute();
    }

    @Test
    public void addPropertiesDoesNothingWhenPropertiesAreEmpty() throws Exception {
        properties.addProperties(INDIVIDUAL, null, Collections.emptyMap());
        verify(handlerMock, never()).addProperties(any(NamedResource.class), anyMap());
        verify(afterMock).execute();
    }

    @Test
    public void removePropertiesCommitsTransactionIfTurnedOn() throws Exception {
        when(adapterMock.getPropertiesHandler()).thenReturn(handlerMock);
        final Map<Assertion, Set<Value<?>>> props = Collections.singletonMap(
                Assertion.createDataPropertyAssertion(URI.create("http://assertion"), false),
                Collections.singleton(new Value<>("String")));
        properties.removeProperties(INDIVIDUAL, null, props);
        verify(handlerMock).removeProperties(INDIVIDUAL, props);
        verify(afterMock).execute();
    }

    @Test
    public void removePropertiesDoesNothingWhenPropertiesAreEmpty() throws Exception {
        properties.removeProperties(INDIVIDUAL, null, Collections.emptyMap());
        verify(handlerMock, never()).removeProperties(any(NamedResource.class), anyMap());
        verify(afterMock).execute();
    }
}

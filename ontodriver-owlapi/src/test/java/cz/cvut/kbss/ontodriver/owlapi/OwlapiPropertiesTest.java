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
package cz.cvut.kbss.ontodriver.owlapi;

import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;
import cz.cvut.kbss.ontodriver.owlapi.util.Procedure;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.net.URI;
import java.util.*;

import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.Mockito.*;

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
        MockitoAnnotations.openMocks(this);
        when(adapterMock.getPropertiesHandler()).thenReturn(handlerMock);
        this.properties = new OwlapiProperties(adapterMock, beforeMock, afterMock);
    }

    @Test
    public void getPropertiesChecksForConnectionActivity() throws Exception {
        final Collection<Axiom<?>> props = new ArrayList<>();
        when(handlerMock.getProperties(eq(INDIVIDUAL), anyBoolean())).thenReturn(props);
        final Collection<Axiom<?>> result = properties.getProperties(INDIVIDUAL, null, true);
        assertSame(props, result);
        verify(beforeMock).execute();
    }

    @Test
    public void addPropertiesCommitsTransactionIfTurnedOn() throws Exception {
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

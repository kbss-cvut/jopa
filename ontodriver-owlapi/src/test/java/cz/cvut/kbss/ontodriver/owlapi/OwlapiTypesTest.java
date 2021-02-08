/**
 * Copyright (C) 2020 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License along with this program. If not, see
 * <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.ontodriver.owlapi;

import cz.cvut.kbss.ontodriver.model.*;
import cz.cvut.kbss.ontodriver.owlapi.util.Procedure;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.net.URI;
import java.util.Collections;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.*;

public class OwlapiTypesTest {

    private static final NamedResource INDIVIDUAL = NamedResource.create("http://krizik.felk.cvut.cz/jopa#individual");

    @Mock
    private TypesHandler typesHandlerMock;

    @Mock
    private OwlapiAdapter adapterMock;

    @Mock
    private Procedure beforeMock;

    private OwlapiTypes types;

    @BeforeEach
    public void setUp() {
        MockitoAnnotations.openMocks(this);
        when(adapterMock.getTypesHandler()).thenReturn(typesHandlerMock);
        this.types = new OwlapiTypes(adapterMock, beforeMock, () -> {
        });
    }

    @Test
    public void getTypesReturnsTypes() throws Exception {
        when(typesHandlerMock.getTypes(INDIVIDUAL, null, false)).thenReturn(Collections.singleton(
                new AxiomImpl<>(INDIVIDUAL, Assertion.createClassAssertion(false),
                        new Value<>(INDIVIDUAL.getIdentifier()))));
        final Set<Axiom<URI>> res = types.getTypes(INDIVIDUAL, null, false);

        assertEquals(1, res.size());
        assertEquals(INDIVIDUAL.getIdentifier(), res.iterator().next().getValue().getValue());
        verify(typesHandlerMock).getTypes(INDIVIDUAL, null, false);
    }

    @Test
    public void addTypesAddsTypes() throws Exception {
        final URI type = URI.create("http://addedType");
        types.addTypes(INDIVIDUAL, null, Collections.singleton(type));

        verify(typesHandlerMock).addTypes(INDIVIDUAL, null, Collections.singleton(type));
    }

    @Test
    public void addTypesOnClosedThrowsException() throws Exception {
        doThrow(IllegalStateException.class).when(beforeMock).execute();

        final URI type = URI.create("http://addedType");
        assertThrows(IllegalStateException.class, () -> types.addTypes(INDIVIDUAL, null, Collections.singleton(type)));
        verify(typesHandlerMock, never()).addTypes(any(), any(), any());
    }

    @Test
    public void addTypesDoesNothingWhenTypesAreEmpty() throws Exception {
        types.addTypes(INDIVIDUAL, null, Collections.emptySet());
        verify(typesHandlerMock, never()).addTypes(any(), any(), any());
    }

    @Test
    public void removeTypesRemovesTypes() throws Exception {
        final URI type = URI.create("http://addedType");
        types.removeTypes(INDIVIDUAL, null, Collections.singleton(type));

        verify(typesHandlerMock).removeTypes(INDIVIDUAL, null, Collections.singleton(type));
    }

    @Test
    public void removeTypesDoesNothingWhenTypesAreEmpty() throws Exception {
        types.removeTypes(INDIVIDUAL, null, Collections.emptySet());
        verify(typesHandlerMock, never()).removeTypes(any(), any(), any());
    }
}

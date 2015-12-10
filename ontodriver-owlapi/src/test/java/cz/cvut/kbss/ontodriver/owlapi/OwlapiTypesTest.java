package cz.cvut.kbss.ontodriver.owlapi;

import cz.cvut.kbss.ontodriver.model.*;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.net.URI;
import java.util.Collections;
import java.util.Set;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.*;

public class OwlapiTypesTest {

    private static final NamedResource INDIVIDUAL = NamedResource.create("http://krizik.felk.cvut.cz/jopa#individual");

    @Mock
    private TypesHandler typesHandlerMock;

    @Mock
    private OwlapiAdapter adapterMock;

    @Mock
    private OwlapiConnection connectionMock;

    private OwlapiTypes types;

    @Before
    public void setUp() {
        MockitoAnnotations.initMocks(this);
        when(adapterMock.getTypesHandler()).thenReturn(typesHandlerMock);
        this.types = new OwlapiTypes(connectionMock, adapterMock);
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

    @Test(expected = IllegalStateException.class)
    public void addTypesOnClosedThrowsException() throws Exception {
        doThrow(IllegalStateException.class).when(connectionMock).ensureOpen();

        final URI type = URI.create("http://addedType");
        try {
            types.addTypes(INDIVIDUAL, null, Collections.singleton(type));
        } finally {
            verify(typesHandlerMock, never()).addTypes(any(), any(), any());
        }
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
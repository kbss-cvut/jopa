package cz.cvut.kbss.ontodriver.sesame;

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

public class SesameTypesTest {

    private static final NamedResource INDIVIDUAL = NamedResource.create("http://krizik.felk.cvut.cz/jopa#individual");

    @Mock
    private SesameConnection connectionMock;
    @Mock
    private SesameAdapter adapterMock;
    @Mock
    private TypesHandler handlerMock;

    private SesameTypes types;

    @Before
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        when(adapterMock.getTypesHandler()).thenReturn(handlerMock);

        this.types = new SesameTypes(connectionMock, adapterMock);
    }

    @Test
    public void testGetTypes() throws Exception {
        when(handlerMock.getTypes(INDIVIDUAL, null, false)).
                thenReturn(Collections.<Axiom<URI>>singleton(
                        new AxiomImpl<>(INDIVIDUAL, Assertion.createClassAssertion(false), new Value<>(INDIVIDUAL.getIdentifier()))));
        final Set<Axiom<URI>> res = types.getTypes(INDIVIDUAL, null, false);
        assertEquals(1, res.size());
        assertEquals(INDIVIDUAL.getIdentifier(), res.iterator().next().getValue().getValue());
        verify(handlerMock).getTypes(INDIVIDUAL, null, false);
    }

    @Test
    public void testAddTypes() throws Exception {
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
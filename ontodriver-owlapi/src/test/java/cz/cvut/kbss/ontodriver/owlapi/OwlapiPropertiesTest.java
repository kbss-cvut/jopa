package cz.cvut.kbss.ontodriver.owlapi;

import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.net.URI;
import java.util.*;

import static org.junit.Assert.assertSame;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyBoolean;
import static org.mockito.Matchers.anyMap;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.*;

public class OwlapiPropertiesTest {

    private static final URI IDENTIFIER = URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#Individual");
    private static final NamedResource INDIVIDUAL = NamedResource.create(IDENTIFIER);

    @Mock
    private PropertiesHandler handlerMock;

    @Mock
    private OwlapiAdapter adapterMock;

    @Mock
    private OwlapiConnection connectionMock;

    private OwlapiProperties properties;

    @Before
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        when(adapterMock.getPropertiesHandler()).thenReturn(handlerMock);
        this.properties = new OwlapiProperties(connectionMock, adapterMock);
    }

    @Test
    public void getPropertiesChecksForConnectionActivity() throws Exception {
        final Collection<Axiom<?>> props = new ArrayList<>();
        when(handlerMock.getProperties(eq(INDIVIDUAL), anyBoolean())).thenReturn(props);
        final Collection<Axiom<?>> result = properties.getProperties(INDIVIDUAL, null, true);
        assertSame(props, result);
        verify(connectionMock).ensureOpen();
    }

    @Test
    public void addPropertiesCommitsTransactionIfTurnedOn() throws Exception {
        final Map<Assertion, Set<Value<?>>> props = Collections.singletonMap(
                Assertion.createDataPropertyAssertion(URI.create("http://assertion"), false),
                Collections.singleton(new Value<>("String")));
        properties.addProperties(INDIVIDUAL, null, props);
        verify(handlerMock).addProperties(INDIVIDUAL, props);
        verify(connectionMock).commitIfAuto();
    }

    @Test
    public void addPropertiesDoesNothingWhenPropertiesAreEmpty() throws Exception {
        properties.addProperties(INDIVIDUAL, null, Collections.emptyMap());
        verify(handlerMock, never()).addProperties(any(NamedResource.class), anyMap());
        verify(connectionMock).commitIfAuto();
    }

    @Test
    public void removePropertiesCommitsTransactionIfTurnedOn() throws Exception {
        final Map<Assertion, Set<Value<?>>> props = Collections.singletonMap(
                Assertion.createDataPropertyAssertion(URI.create("http://assertion"), false),
                Collections.singleton(new Value<>("String")));
        properties.removeProperties(INDIVIDUAL, null, props);
        verify(handlerMock).removeProperties(INDIVIDUAL, props);
        verify(connectionMock).commitIfAuto();
    }

    @Test
    public void removePropertiesDoesNothingWhenPropertiesAreEmpty() throws Exception {
        properties.removeProperties(INDIVIDUAL, null, Collections.emptyMap());
        verify(handlerMock, never()).removeProperties(any(NamedResource.class), anyMap());
        verify(connectionMock).commitIfAuto();
    }
}
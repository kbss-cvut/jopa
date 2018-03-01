package cz.cvut.kbss.ontodriver.jena;

import cz.cvut.kbss.ontodriver.jena.environment.Generator;
import cz.cvut.kbss.ontodriver.jena.util.Procedure;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InOrder;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import static org.mockito.Mockito.*;

public class JenaPropertiesTest {

    private static final NamedResource SUBJECT = NamedResource.create(Generator.generateUri());

    @Mock
    private PropertiesHandler propertiesHandler;

    @Mock
    private Procedure beforeMock;

    @Mock
    private Procedure afterMock;

    private JenaProperties properties;

    @Before
    public void setUp() {
        MockitoAnnotations.initMocks(this);
        final JenaAdapter adapterMock = mock(JenaAdapter.class);
        when(adapterMock.propertiesHandler()).thenReturn(propertiesHandler);
        this.properties = new JenaProperties(adapterMock, beforeMock, afterMock);
    }

    @Test
    public void getPropertiesInvokesPropertiesHandler() throws Exception {
        properties.getProperties(SUBJECT, null, false);
        verify(propertiesHandler).getProperties(SUBJECT, null, false);
    }

    @Test
    public void getPropertiesInvokesBeforeCallbackPriorToPropertiesLoading() throws Exception {
        properties.getProperties(SUBJECT, null, false);
        final InOrder inOrder = Mockito.inOrder(beforeMock, propertiesHandler);
        inOrder.verify(beforeMock).execute();
        inOrder.verify(propertiesHandler).getProperties(SUBJECT, null, false);
    }

    @Test
    public void addPropertiesInvokesPropertiesHandler() throws Exception {
        final Map<Assertion, Set<Value<?>>> props = initValues();
        properties.addProperties(SUBJECT, null, props);
        verify(propertiesHandler).addProperties(SUBJECT, null, props);
    }

    private Map<Assertion, Set<Value<?>>> initValues() {
        final Assertion a = Assertion.createDataPropertyAssertion(Generator.generateUri(), false);
        final Map<Assertion, Set<Value<?>>> props = new HashMap<>();
        props.put(a, Collections.singleton(new Value<>(117)));
        return props;
    }

    @Test
    public void addPropertiesInvokesCallbacks() throws Exception {
        final Map<Assertion, Set<Value<?>>> props = initValues();
        properties.addProperties(SUBJECT, null, props);
        final InOrder inOrder = Mockito.inOrder(beforeMock, propertiesHandler, afterMock);
        inOrder.verify(beforeMock).execute();
        inOrder.verify(propertiesHandler).addProperties(SUBJECT, null, props);
        inOrder.verify(afterMock).execute();
    }

    @Test
    public void removePropertiesInvokesPropertiesHandler() throws Exception {
        final Map<Assertion, Set<Value<?>>> props = initValues();
        properties.removeProperties(SUBJECT, null, props);
        verify(propertiesHandler).removeProperties(SUBJECT, null, props);
    }

    @Test
    public void removePropertiesInvokesCallbacks() throws Exception {
        final Map<Assertion, Set<Value<?>>> props = initValues();
        properties.removeProperties(SUBJECT, null, props);
        final InOrder inOrder = Mockito.inOrder(beforeMock, propertiesHandler, afterMock);
        inOrder.verify(beforeMock).execute();
        inOrder.verify(propertiesHandler).removeProperties(SUBJECT, null, props);
        inOrder.verify(afterMock).execute();
    }
}
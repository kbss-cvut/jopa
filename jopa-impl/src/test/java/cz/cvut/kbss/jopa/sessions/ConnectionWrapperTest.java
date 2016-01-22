package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.oom.ObjectOntologyMapper;
import cz.cvut.kbss.ontodriver.Connection;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.lang.reflect.Field;

import static org.junit.Assert.assertFalse;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

public class ConnectionWrapperTest {

    @Mock
    private Connection connectionMock;
    @Mock
    private ObjectOntologyMapper oomMock;

    private ConnectionWrapper connectionWrapper;

    @Before
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        this.connectionWrapper = new ConnectionWrapper(connectionMock);
        final Field oomField = ConnectionWrapper.class.getDeclaredField("mapper");
        oomField.setAccessible(true);
        oomField.set(connectionWrapper, oomMock);
    }

    @Test
    public void containsReturnsFalseForNullIdentifier() {
        final boolean res = connectionWrapper.contains(null, OWLClassA.class, new EntityDescriptor());
        assertFalse(res);
        verify(oomMock, never()).containsEntity(any(), any(), any());
    }

}
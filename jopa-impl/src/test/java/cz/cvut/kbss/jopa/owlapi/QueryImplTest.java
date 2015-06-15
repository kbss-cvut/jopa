package cz.cvut.kbss.jopa.owlapi;

import cz.cvut.kbss.jopa.sessions.ConnectionWrapper;
import org.junit.Before;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import static org.junit.Assert.*;

/**
 * @author kidney
 */
public class QueryImplTest {

    @Mock
    private ConnectionWrapper connectionWrapperMock;

    private QueryImpl query;

    @Before
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
    }

    // TODO
}
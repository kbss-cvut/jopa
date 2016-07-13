package cz.cvut.kbss.jopa.test.integration;

import cz.cvut.kbss.jopa.model.query.Query;
import cz.cvut.kbss.jopa.model.query.TypedQuery;
import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.Vocabulary;
import cz.cvut.kbss.jopa.test.integration.environment.TestDataSource;
import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.Statement;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import static org.junit.Assert.assertNotNull;
import static org.mockito.Matchers.anyString;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class NamedQueryTest extends IntegrationTestBase {

    @Mock
    private Connection connectionMock;
    @Mock
    private Statement statementMock;
    @Mock
    private ResultSet resultSetMock;

    @Before
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        super.setUp();
        final TestDataSource ds = getDataSource();
        ds.setConnection(connectionMock);
        when(connectionMock.createStatement()).thenReturn(statementMock);
    }

    @Test
    public void testCreateTypedNamedNativeQuery() throws Exception {
        when(statementMock.executeQuery(anyString())).thenReturn(resultSetMock);
        final TypedQuery<OWLClassA> query = em.createNamedQuery("OWLClassA.findAll", OWLClassA.class);
        assertNotNull(query);
    }

    @Test
    public void testNamedNativeQueryWithParameterReplace() throws Exception {
        when(statementMock.executeQuery(anyString())).thenReturn(resultSetMock);
        final Query query = em.createNamedQuery("OWLClassA.findByString");
        assertNotNull(query);
        query.setParameter("str", "Test", "en").getResultList();
        verify(statementMock)
                .executeQuery("SELECT ?x WHERE { ?x <" + Vocabulary.pAStringAttribute + "> \"Test\"@en . }");
    }

    @Test(expected = IllegalArgumentException.class)
    public void createUnknownNamedNativeQueryThrowsIllegalArgument() throws Exception {
        em.createNamedQuery("findAll");
    }
}

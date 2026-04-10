package cz.cvut.kbss.jopa.test.integration;

import cz.cvut.kbss.jopa.model.EntityGraph;
import cz.cvut.kbss.jopa.model.Subgraph;
import cz.cvut.kbss.jopa.query.QueryHints;
import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.OWLClassD;
import cz.cvut.kbss.jopa.test.OWLClassS;
import cz.cvut.kbss.jopa.test.OWLClassU;
import cz.cvut.kbss.jopa.test.Vocabulary;
import cz.cvut.kbss.jopa.test.environment.Generators;
import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.Statement;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.iteration.ResultRow;
import cz.cvut.kbss.ontodriver.iteration.ResultSetIterator;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;

import java.net.URI;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
public class FetchGraphEntityLoadingTest extends IntegrationTestBase {

    @BeforeEach
    public void setUp() throws Exception {
        super.setUp();
    }

    @Test
    void fetchGraphBasedQueryResultLoadingWorks() throws OntoDriverException {
        when(connectionMock.getRepositoryMetadata()).thenReturn(() -> "");
        final OWLClassD expected = new OWLClassD(Generators.generateUri());
        final OWLClassA expectedA = Generators.generateOwlClassA();
        expected.setOwlClassA(expectedA);
        mockQueryResultSet(expected);
        final EntityGraph<OWLClassD> fetchGraph = em.createEntityGraph(OWLClassD.class);
        final Subgraph<OWLClassA> subgraph = fetchGraph.addSubgraph("owlClassA");
        subgraph.addAttributeNodes("stringAttribute");

        final List<OWLClassD> result = em.createQuery("SELECT d FROM OWLClassD d", OWLClassD.class)
                                         .setHint(QueryHints.FETCH_GRAPH, fetchGraph)
                                         .getResultList();
        assertEquals(1, result.size());
        assertEquals(expected.getUri(), result.get(0).getUri());
        assertNotNull(result.get(0).getOwlClassA());
        assertEquals(expectedA.getUri(), result.get(0).getOwlClassA().getUri());
        assertEquals(expectedA.getStringAttribute(), result.get(0).getOwlClassA().getStringAttribute());
        verify(connectionMock, never()).find(any());
    }

    private void mockQueryResultSet(OWLClassD expected) throws OntoDriverException {
        final Statement statementMock = mock(Statement.class);
        final ResultSet resultSetMock = mock(ResultSet.class);
        final ResultSetIterator resultSetIteratorMock = mock(ResultSetIterator.class);
        final ResultRow resultRowMock = mock(ResultRow.class);
        when(connectionMock.createStatement()).thenReturn(statementMock);
        when(statementMock.executeQuery(anyString())).thenReturn(resultSetMock);
        when(resultSetMock.iterator()).thenReturn(resultSetIteratorMock);
        when(resultSetIteratorMock.next()).thenReturn(resultRowMock);
        when(resultSetIteratorMock.hasNext()).thenReturn(true, false);
        when(resultRowMock.getColumnCount()).thenReturn(5);
        when(resultRowMock.isBound("x")).thenReturn(true);
        when(resultRowMock.getObject("x", URI.class)).thenReturn(expected.getUri());
        when(resultRowMock.getObject(0, URI.class)).thenReturn(expected.getUri());
        when(resultRowMock.isBound("x_types")).thenReturn(true);
        when(resultRowMock.getObject("x_types")).thenReturn(URI.create(Vocabulary.C_OWL_CLASS_D));
        when(resultRowMock.isBound("x_owlClassA")).thenReturn(true);
        when(resultRowMock.getObject("x_owlClassA")).thenReturn(expected.getOwlClassA().getUri());
        when(resultRowMock.getObject("x_owlClassA", URI.class)).thenReturn(expected.getOwlClassA().getUri());
        when(resultRowMock.isBound("x_owlClassA_types")).thenReturn(true);
        when(resultRowMock.getObject("x_owlClassA_types")).thenReturn(URI.create(Vocabulary.C_OWL_CLASS_A));
        when(resultRowMock.isBound("x_owlClassA_stringAttribute")).thenReturn(true);
        when(resultRowMock.getObject("x_owlClassA_stringAttribute")).thenReturn(expected.getOwlClassA()
                                                                                        .getStringAttribute());
    }

    @Test
    void fetchGraphBasedQueryResultLoadingDoesNotLoadAttributesNotSpecifiedInFetchGraph() throws OntoDriverException {
        when(connectionMock.getRepositoryMetadata()).thenReturn(() -> "");
        final OWLClassU expected = new OWLClassU();
        expected.setUri(Generators.generateUri());
        expected.setName("test");
        expected.setDescription("test description");
        final OWLClassS reference = new OWLClassS();
        reference.setUri(Generators.generateUri());
        reference.setName("test reference");
        reference.setDescription("test reference description");
        expected.setOwlClassS(reference);
        mockQueryResultSet(expected);
        final EntityGraph<OWLClassU> fetchGraph = em.createEntityGraph(OWLClassU.class);
        fetchGraph.addAttributeNodes("name");
        final Subgraph<OWLClassS> subgraph = fetchGraph.addSubgraph("owlClassS");
        subgraph.addAttributeNodes("name");

        final List<OWLClassU> result = em.createQuery("SELECT u FROM OWLClassU u", OWLClassU.class)
                                         .setHint(QueryHints.FETCH_GRAPH, fetchGraph)
                                         .getResultList();
        assertEquals(1, result.size());
        assertEquals(expected.getUri(), result.get(0).getUri());
        assertEquals(expected.getName(), result.get(0).getName());
        assertNull(result.get(0).getDescription());
        assertNotNull(result.get(0).getOwlClassS());
        assertEquals(reference.getUri(), result.get(0).getOwlClassS().getUri());
        assertEquals(reference.getName(), result.get(0).getOwlClassS().getName());
        assertNull(result.get(0).getOwlClassS().getDescription());
    }

    private void mockQueryResultSet(OWLClassU expected) throws OntoDriverException {
        final Statement statementMock = mock(Statement.class);
        final ResultSet resultSetMock = mock(ResultSet.class);
        final ResultSetIterator resultSetIteratorMock = mock(ResultSetIterator.class);
        final ResultRow resultRowMock = mock(ResultRow.class);
        when(connectionMock.createStatement()).thenReturn(statementMock);
        when(statementMock.executeQuery(anyString())).thenReturn(resultSetMock);
        when(resultSetMock.iterator()).thenReturn(resultSetIteratorMock);
        when(resultSetIteratorMock.next()).thenReturn(resultRowMock);
        when(resultSetIteratorMock.hasNext()).thenReturn(true, false);
        when(resultRowMock.getColumnCount()).thenReturn(6);
        when(resultRowMock.isBound("x")).thenReturn(true);
        when(resultRowMock.getObject("x", URI.class)).thenReturn(expected.getUri());
        when(resultRowMock.getObject(0, URI.class)).thenReturn(expected.getUri());
        when(resultRowMock.isBound("x_name")).thenReturn(true);
        when(resultRowMock.getObject("x_name")).thenReturn(expected.getName());
        when(resultRowMock.isBound("x_types")).thenReturn(true);
        when(resultRowMock.getObject("x_types")).thenReturn(URI.create(Vocabulary.C_OWL_CLASS_U));
        when(resultRowMock.isBound("x_owlClassS")).thenReturn(true);
        when(resultRowMock.getObject("x_owlClassS")).thenReturn(expected.getOwlClassS().getUri());
        when(resultRowMock.getObject("x_owlClassS", URI.class)).thenReturn(expected.getOwlClassS().getUri());
        when(resultRowMock.isBound("x_owlClassS_name")).thenReturn(true);
        when(resultRowMock.getObject("x_owlClassS_name")).thenReturn(expected.getOwlClassS().getName());
        when(resultRowMock.isBound("x_owlClassS_types")).thenReturn(true);
        when(resultRowMock.getObject("x_owlClassS_types")).thenReturn(URI.create(Vocabulary.C_OWL_CLASS_S));
    }
}

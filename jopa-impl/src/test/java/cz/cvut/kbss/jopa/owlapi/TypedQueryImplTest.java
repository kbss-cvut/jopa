package cz.cvut.kbss.jopa.owlapi;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.exceptions.NoResultException;
import cz.cvut.kbss.jopa.exceptions.NoUniqueResultException;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.query.TypedQuery;
import cz.cvut.kbss.jopa.sessions.ConnectionWrapper;
import cz.cvut.kbss.jopa.sessions.MetamodelProvider;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;
import cz.cvut.kbss.ontodriver_new.ResultSet;
import cz.cvut.kbss.ontodriver_new.Statement;
import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;

import static org.junit.Assert.*;
import static org.mockito.Matchers.*;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * @author kidney
 */
public class TypedQueryImplTest {

    private static final String SELECT_ENTITY_QUERY = "SELECT ?x WHERE { ?x a <http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassA> . }";
    private static final String ASK_BOOLEAN_QUERY = "ASK { ?x a <http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassA> . }";

    @Mock
    private UnitOfWork uowMock;

    @Mock
    private MetamodelProvider metamodelProviderMock;

    @Mock
    private ConnectionWrapper connectionMock;

    @Mock
    private Statement statementMock;

    @Mock
    private ResultSet resultSetMock;

    @Before
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        when(connectionMock.createStatement()).thenReturn(statementMock);
        when(statementMock.executeQuery(anyString())).thenReturn(resultSetMock);
        when(statementMock.executeQuery(anyString(), anyVararg())).thenReturn(resultSetMock);
        when(metamodelProviderMock.isTypeManaged(OWLClassA.class)).thenReturn(true);
    }

    @Test
    public void getResultListWithEntityTypeReturnsEntities() throws Exception {
        final TypedQuery<OWLClassA> query = create(OWLClassA.class, SELECT_ENTITY_QUERY);
        final List<String> uris = initDataForQuery(5);
        final List<OWLClassA> res = query.getResultList();
        verifyResults(uris, res, 5);
    }

    private <T> TypedQuery<T> create(Class<T> type, String query) {
        final TypedQueryImpl<T> tq = new TypedQueryImpl<>(query, type, connectionMock, metamodelProviderMock);
        tq.setUnitOfWork(uowMock);
        return tq;
    }

    private List<String> initDataForQuery(int count) throws Exception {
        final List<String> uris = new ArrayList<>(count);
        final List<Boolean> hasNext = new ArrayList<>(count);
        for (int i = 0; i < count; i++) {
            final String u = "http://uri" + i;
            uris.add(u);
            when(uowMock.readObject(eq(OWLClassA.class), eq(URI.create(u)), any(Descriptor.class))).thenReturn(new OWLClassA(URI.create(u)));
            hasNext.add(true);
        }
        hasNext.add(false);
        when(resultSetMock.getString(0)).thenReturn(uris.get(0), uris.subList(1, uris.size()).toArray(new String[count]));
        when(resultSetMock.hasNext()).thenReturn(hasNext.get(0), hasNext.subList(1, hasNext.size()).toArray(new Boolean[count]));
        return uris;
    }

    private void verifyResults(List<String> uris, List<OWLClassA> results, int expectedCount) {
        assert expectedCount <= uris.size();
        assertEquals(expectedCount, results.size());
        for (int i = 0; i < expectedCount; i++) {
            assertEquals(uris.get(i), results.get(i).getUri().toString());
        }
    }

    @Test
    public void getSingleResultWithEntityTypeReturnsCorrectResult() throws Exception {
        final TypedQuery<OWLClassA> query = create(OWLClassA.class, SELECT_ENTITY_QUERY);
        final List<String> uris = initDataForQuery(1);
        final OWLClassA res = query.getSingleResult();
        assertNotNull(res);
        assertEquals(uris.get(0), res.getUri().toString());
    }

    @Test
    public void setMaxResultsReturnsSpecifiedMaxResults() throws Exception {
        final TypedQuery<OWLClassA> query = create(OWLClassA.class, SELECT_ENTITY_QUERY);
        final int count = 10;
        final List<String> uris = initDataForQuery(count);
        final List<OWLClassA> res = query.setMaxResults(count - 5).getResultList();
        verifyResults(uris, res, count - 5);
    }

    @Test
    public void setMaxResultsLargerReturnsAllResults() throws Exception {
        final TypedQuery<OWLClassA> query = create(OWLClassA.class, SELECT_ENTITY_QUERY);
        final int count = 10;
        final List<String> uris = initDataForQuery(count);
        final List<OWLClassA> res = query.setMaxResults(count + 5).getResultList();
        verifyResults(uris, res, count);
    }

    @Test
    public void returnsEmptyListWhenMaxResultsIsSetToZero() throws Exception {
        final TypedQuery<OWLClassA> query = create(OWLClassA.class, SELECT_ENTITY_QUERY);
        initDataForQuery(2);
        final List<OWLClassA> res = query.setMaxResults(0).getResultList();
        assertNotNull(res);
        assertTrue(res.isEmpty());
    }

    @Test(expected = IllegalArgumentException.class)
    public void throwsExceptionWhenNegativeIsUsedForSetMaxResults() throws Exception {
        final TypedQuery<OWLClassA> query = create(OWLClassA.class, SELECT_ENTITY_QUERY);
        query.setMaxResults(-1).getResultList();
    }

    @Test
    public void addingContextsExecutesTheQueryInContexts() throws Exception {
        final TypedQuery<OWLClassA> query = create(OWLClassA.class, SELECT_ENTITY_QUERY);
        final List<String> uris = initDataForQuery(1);
        final URI contextOne = URI.create("http://contextOne");
        final URI contextTwo = URI.create("http://contextTwo");
        query.addContext(contextOne).addContext(contextTwo);

        final List<OWLClassA> res = query.getResultList();
        assertNotNull(res);
        verifyResults(uris, res, 1);
        final ArgumentCaptor<URI[]> captor = ArgumentCaptor.forClass(URI[].class);
        verify(statementMock).executeQuery(anyString(), captor.capture());
        final List<URI[]> contexts = captor.getAllValues();
        assertEquals(2, contexts.size());
        // This works, because Mockito captor returns the values as singular URIs instead of a field of URIs
        assertTrue(contexts.contains(contextOne));
        assertTrue(contexts.contains(contextTwo));
    }

    @Test(expected = NoResultException.class)
    public void throwsNoResultExceptionWhenThereIsNoResultForGetSingle() throws Exception {
        final TypedQuery<OWLClassA> query = create(OWLClassA.class, SELECT_ENTITY_QUERY);
        query.getSingleResult();
    }

    @Test(expected = NoUniqueResultException.class)
    public void throwsNoSingleResultExceptionWhenThereAreMultipleResultsForGetSingle() throws Exception {
        final TypedQuery<OWLClassA> query = create(OWLClassA.class, SELECT_ENTITY_QUERY);
        initDataForQuery(5);
        query.getSingleResult();
    }

    @Test(expected = IllegalStateException.class)
    public void throwsExceptionWhenLoadingEntityWithoutUoWSet() throws Exception {
        final TypedQueryImpl<OWLClassA> query = new TypedQueryImpl<>(SELECT_ENTITY_QUERY, OWLClassA.class, connectionMock, metamodelProviderMock);
        initDataForQuery(5);
        query.getResultList();
    }

    @Test
    public void askQueryReturnsSingleBoolean() throws Exception {
        final TypedQuery<Boolean> query = create(Boolean.class, ASK_BOOLEAN_QUERY);
        initAskQueryData(true);
        final Boolean result = query.getSingleResult();
        assertNotNull(result);
        assertTrue(result);
        verify(uowMock, never()).readObject(eq(Boolean.class), anyObject(), any(Descriptor.class));
    }

    private void initAskQueryData(boolean result) throws Exception {
        when(resultSetMock.hasNext()).thenReturn(true, false);
        when(resultSetMock.getObject(0, Boolean.class)).thenReturn(result);
    }
}
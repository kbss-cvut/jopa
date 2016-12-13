/**
 * Copyright (C) 2016 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.model;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.exceptions.NoResultException;
import cz.cvut.kbss.jopa.exceptions.NoUniqueResultException;
import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.query.Query;
import cz.cvut.kbss.jopa.model.query.TypedQuery;
import cz.cvut.kbss.jopa.query.sparql.SparqlQueryHolder;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentCaptor;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;

import static org.junit.Assert.*;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyObject;
import static org.mockito.Matchers.anyString;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.*;

public class TypedQueryImplTest extends QueryTestBase {

    private static final String SELECT_ENTITY_QUERY = "SELECT ?x WHERE { ?x a <http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassA> . }";
    private static final String ASK_BOOLEAN_QUERY = "ASK { ?x a <http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassA> . }";

    @Override
    TypedQuery<?> createQuery(String query, Class<?> resultType) {
        return queryFactory.createNativeQuery(query, resultType);
    }

    @Before
    public void setUp() throws Exception {
        super.setUp();
        when(uowMock.isTypeManaged(OWLClassA.class)).thenReturn(true);
    }

    @Test
    public void getResultListWithEntityTypeReturnsEntities() throws Exception {
        final TypedQuery<OWLClassA> query = create(OWLClassA.class, SELECT_ENTITY_QUERY);
        final List<String> uris = initDataForQuery(5);
        final List<OWLClassA> res = query.getResultList();
        verifyResults(uris, res, 5);
    }

    private <T> TypedQuery<T> create(Class<T> type, String query) {
        return queryFactory.createNativeQuery(query, type);
    }

    private List<String> initDataForQuery(int count) throws Exception {
        final List<String> uris = new ArrayList<>(count);
        final List<Boolean> hasNext = new ArrayList<>(count);
        for (int i = 0; i < count; i++) {
            final String u = "http://uri" + i;
            uris.add(u);
            when(uowMock.readObject(eq(OWLClassA.class), eq(URI.create(u)), any(Descriptor.class)))
                    .thenReturn(new OWLClassA(URI.create(u)));
            hasNext.add(true);
        }
        hasNext.add(false);
        when(resultSetMock.getString(0))
                .thenReturn(uris.get(0), uris.subList(1, uris.size()).toArray(new String[count]));
        when(resultSetMock.hasNext())
                .thenReturn(hasNext.get(0), hasNext.subList(1, hasNext.size()).toArray(new Boolean[count]));
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
        final TypedQueryImpl<OWLClassA> query = new TypedQueryImpl<>(mock(SparqlQueryHolder.class), OWLClassA.class,
                connectionWrapperMock, uowMock);
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

    @Test
    public void executeUpdateRunsUpdateOnConnection() throws Exception {
        final String update = "INSERT { ?inst ?property ?newValue . } " +
                "DELETE { ?inst ?property ?origValue . } WHERE {" +
                "?inst ?property ?origValue . }";
        final TypedQuery<Void> q = queryFactory.createNativeQuery(update, Void.class);
        q.executeUpdate();
        verify(statementMock).executeUpdate(update);
    }

    @Test
    public void executeUpdateThrowsPersistenceExceptionWhenOntoDriverExceptionIsThrown() throws Exception {
        thrown.expect(OWLPersistenceException.class);
        thrown.expectMessage("Exception caught when evaluating query " + UPDATE_QUERY);
        doThrow(new OntoDriverException()).when(statementMock).executeUpdate(UPDATE_QUERY);
        final Query q = queryFactory.createNativeQuery(UPDATE_QUERY, Void.class);
        q.executeUpdate();
    }
}
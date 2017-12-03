/**
 * Copyright (C) 2016 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License along with this program. If not, see
 * <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.model;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.exceptions.NoResultException;
import cz.cvut.kbss.jopa.exceptions.NoUniqueResultException;
import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.query.Query;
import cz.cvut.kbss.jopa.model.query.TypedQuery;
import cz.cvut.kbss.jopa.query.QueryParameter;
import cz.cvut.kbss.jopa.query.sparql.SparqlQueryHolder;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import org.junit.Before;
import org.junit.Test;

import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static org.junit.Assert.*;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyObject;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.anyInt;
import static org.mockito.Mockito.*;

public class TypedQueryImplTest extends QueryTestBase {

    private static final String ASK_BOOLEAN_QUERY =
            "ASK { ?x a <http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassA> . }";

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
        final TypedQuery<OWLClassA> query = create(OWLClassA.class, SELECT_QUERY);
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
        final TypedQuery<OWLClassA> query = create(OWLClassA.class, SELECT_QUERY);
        final List<String> uris = initDataForQuery(1);
        final OWLClassA res = query.getSingleResult();
        assertNotNull(res);
        assertEquals(uris.get(0), res.getUri().toString());
    }

    @Test
    public void setMaxResultsReturnsSpecifiedMaxResults() throws Exception {
        final TypedQuery<OWLClassA> query = create(OWLClassA.class, SELECT_QUERY);
        final int count = 10;
        final int expectedCount = 5;
        final List<String> uris = initDataForQuery(count);
        final List<OWLClassA> res = query.setMaxResults(expectedCount).getResultList();
        assertEquals(expectedCount, query.getMaxResults());
        verifyResults(uris, res, expectedCount);
    }

    @Test
    public void setMaxResultsLargerReturnsAllResults() throws Exception {
        final TypedQuery<OWLClassA> query = create(OWLClassA.class, SELECT_QUERY);
        final int count = 10;
        final List<String> uris = initDataForQuery(count);
        final List<OWLClassA> res = query.setMaxResults(count + 5).getResultList();
        verifyResults(uris, res, count);
    }

    @Test
    public void returnsEmptyListWhenMaxResultsIsSetToZero() throws Exception {
        final TypedQuery<OWLClassA> query = create(OWLClassA.class, SELECT_QUERY);
        initDataForQuery(2);
        final List<OWLClassA> res = query.setMaxResults(0).getResultList();
        assertNotNull(res);
        assertTrue(res.isEmpty());
    }

    @Test(expected = IllegalArgumentException.class)
    public void throwsExceptionWhenNegativeIsUsedForSetMaxResults() throws Exception {
        final TypedQuery<OWLClassA> query = create(OWLClassA.class, SELECT_QUERY);
        query.setMaxResults(-1).getResultList();
    }

    @Test(expected = NoResultException.class)
    public void throwsNoResultExceptionWhenThereIsNoResultForGetSingle() throws Exception {
        final TypedQuery<OWLClassA> query = create(OWLClassA.class, SELECT_QUERY);
        query.getSingleResult();
    }

    @Test(expected = NoUniqueResultException.class)
    public void throwsNoSingleResultExceptionWhenThereAreMultipleResultsForGetSingle() throws Exception {
        final TypedQuery<OWLClassA> query = create(OWLClassA.class, SELECT_QUERY);
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

    @Test
    public void getResultListSkipsValuesWhichCannotBeLoadedAsEntities() throws Exception {
        when(resultSetMock.hasNext()).thenReturn(true, true, false);
        final List<String> uris = Arrays.asList(Generators.createIndividualIdentifier().toString(),
                Generators.createIndividualIdentifier().toString());
        when(resultSetMock.getString(0)).thenReturn(uris.get(0), uris.get(1));
        when(uowMock.readObject(eq(OWLClassA.class), eq(URI.create(uris.get(0))), any(Descriptor.class)))
                .thenReturn(new OWLClassA(URI.create(uris.get(0))));

        final TypedQuery<OWLClassA> q = queryFactory.createNativeQuery(SELECT_QUERY, OWLClassA.class);
        final List<OWLClassA> result = q.getResultList();
        assertEquals(1, result.size());
    }

    @Test
    public void exceptionInExecuteUpdateInvokesRollbackMarker() throws Exception {
        doThrow(new OntoDriverException()).when(statementMock).executeUpdate(UPDATE_QUERY);
        final TypedQueryImpl<Void> q = queryFactory.createNativeQuery(UPDATE_QUERY, Void.class);
        runAndVerifyHandlerInvocation(q, q::executeUpdate);
    }

    private <T> void runAndVerifyHandlerInvocation(TypedQueryImpl<T> query, Runnable method) {
        query.setRollbackOnlyMarker(handler);
        try {
            method.run();
        } catch (RuntimeException e) {
            // Swallow the exception
        }
        verify(handler).execute();
    }

    @Test
    public void runtimeExceptionInExecuteUpdateInvokesRollbackMarker() throws Exception {
        doThrow(OWLPersistenceException.class).when(statementMock).executeUpdate(UPDATE_QUERY);
        final TypedQueryImpl<Void> q = queryFactory.createNativeQuery(UPDATE_QUERY, Void.class);
        runAndVerifyHandlerInvocation(q, q::executeUpdate);
    }

    @Test
    public void exceptionInGetResultListInvokesRollbackMarker() throws Exception {
        doThrow(OntoDriverException.class).when(statementMock).executeQuery(SELECT_QUERY);
        final TypedQueryImpl<OWLClassA> q = queryFactory.createNativeQuery(SELECT_QUERY, OWLClassA.class);
        runAndVerifyHandlerInvocation(q, q::getResultList);
    }

    @Test
    public void runtimeExceptionInGetResultListInvokesRollbackMarker() throws Exception {
        doThrow(OWLPersistenceException.class).when(statementMock).executeQuery(SELECT_QUERY);
        final TypedQueryImpl<OWLClassA> q = queryFactory.createNativeQuery(SELECT_QUERY, OWLClassA.class);
        runAndVerifyHandlerInvocation(q, q::getResultList);
    }

    @Test
    public void exceptionInGetSingleResultInvokesRollbackMarker() throws Exception {
        doThrow(OntoDriverException.class).when(statementMock).executeQuery(SELECT_QUERY);
        final TypedQueryImpl<OWLClassA> q = queryFactory.createNativeQuery(SELECT_QUERY, OWLClassA.class);
        runAndVerifyHandlerInvocation(q, q::getSingleResult);
    }

    @Test
    public void runtimeExceptionInGetSingleResultInvokesRollbackMarker() throws Exception {
        doThrow(OWLPersistenceException.class).when(statementMock).executeQuery(SELECT_QUERY);
        final TypedQueryImpl<OWLClassA> q = queryFactory.createNativeQuery(SELECT_QUERY, OWLClassA.class);
        runAndVerifyHandlerInvocation(q, q::getSingleResult);
    }

    @Test
    public void exceptionInSetMaxResultsInvokesRollbackMarker() throws Exception {
        final TypedQueryImpl<OWLClassA> q = queryWithRollbackMarker(SELECT_QUERY, OWLClassA.class);
        try {
            q.setMaxResults(-1);
        } catch (RuntimeException e) {
            // Swallow the exception
        }
        verify(handler).execute();
    }

    private <T> TypedQueryImpl<T> queryWithRollbackMarker(String query, Class<T> cls) {
        final TypedQueryImpl<T> q = queryFactory.createNativeQuery(query, cls);
        q.setRollbackOnlyMarker(handler);
        return q;
    }

    @Test
    public void exceptionInSetParameterByPositionInvokesRollbackMarker() {
        final TypedQueryImpl<OWLClassA> q = queryWithRollbackMarker(SELECT_QUERY, OWLClassA.class);
        try {
            q.setParameter(117, 117);
        } catch (RuntimeException e) {
            // Swallow the exception
        }
        verify(handler).execute();
    }

    @Test
    public void exceptionInSetStringParameterByPositionInvokesRollbackMarker() {
        final TypedQueryImpl<OWLClassA> q = queryWithRollbackMarker(SELECT_QUERY, OWLClassA.class);
        try {
            q.setParameter(117, "A", "en");
        } catch (RuntimeException e) {
            // Swallow the exception
        }
        verify(handler).execute();
    }

    @Test
    public void exceptionInSetParameterByNameInvokesRollbackMarker() {
        final TypedQueryImpl<OWLClassA> q = queryWithRollbackMarker(SELECT_QUERY, OWLClassA.class);
        try {
            q.setParameter("a", 117);
        } catch (RuntimeException e) {
            // Swallow the exception
        }
        verify(handler).execute();
    }

    @Test
    public void exceptionInSetStringParameterByNameInvokesRollbackMarker() {
        final TypedQueryImpl<OWLClassA> q = queryWithRollbackMarker(SELECT_QUERY, OWLClassA.class);
        try {
            q.setParameter("a", "A", "en");
        } catch (RuntimeException e) {
            // Swallow the exception
        }
        verify(handler).execute();
    }

    @Test
    public void exceptionInSetParameterByParameterInvokesRollbackMarker() {
        final TypedQueryImpl<OWLClassA> q = queryWithRollbackMarker(SELECT_QUERY, OWLClassA.class);
        try {
            q.setParameter(new QueryParameter<>(117), 117);
        } catch (RuntimeException e) {
            // Swallow the exception
        }
        verify(handler).execute();
    }

    @Test
    public void exceptionInSetStringParameterByParameterInvokesRollbackMarker() {
        final TypedQueryImpl<OWLClassA> q = queryWithRollbackMarker(SELECT_QUERY, OWLClassA.class);
        try {
            q.setParameter(new QueryParameter<>(117), "A", "en");
        } catch (RuntimeException e) {
            // Swallow the exception
        }
        verify(handler).execute();
    }

    @Test
    public void setDescriptorPassesDescriptorToInstanceLoading() throws Exception {
        final TypedQuery<OWLClassA> query = create(OWLClassA.class, SELECT_QUERY);
        final int count = 10;
        final List<String> uris = initDataForQuery(count);
        final Descriptor descriptor = new EntityDescriptor(URI.create("http://contextOne"));
        query.setDescriptor(descriptor).getResultList();
        for (String uri : uris) {
            verify(uowMock).readObject(OWLClassA.class, URI.create(uri), descriptor);
        }
    }

    @Test
    public void setFirstResultOffsetsQueryResultStartToSpecifiedPosition() throws Exception {
        final TypedQuery<OWLClassA> q = queryFactory.createNativeQuery(SELECT_QUERY, OWLClassA.class);
        when(resultSetMock.getColumnCount()).thenReturn(1);
        // Three results
        when(resultSetMock.hasNext()).thenReturn(true).thenReturn(true).thenReturn(true).thenReturn(false);
        final URI uri = Generators.createIndividualIdentifier();
        final OWLClassA a = Generators.generateOwlClassAInstance();
        when(resultSetMock.getString(anyInt())).thenReturn(uri.toString());
        when(uowMock.readObject(eq(OWLClassA.class), eq(uri), any(Descriptor.class))).thenReturn(a);
        q.setFirstResult(2);
        assertEquals(2, q.getFirstResult());
        final List<OWLClassA> result = q.getResultList();
        assertEquals(1, result.size());
        assertEquals(a, result.get(0));
        verify(resultSetMock).getString(anyInt());
        verify(uowMock).readObject(eq(OWLClassA.class), any(), any());
    }
}
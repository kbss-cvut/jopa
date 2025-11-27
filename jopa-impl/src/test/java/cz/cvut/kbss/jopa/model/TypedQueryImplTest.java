/*
 * JOPA
 * Copyright (C) 2025 Czech Technical University in Prague
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */
package cz.cvut.kbss.jopa.model;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassB;
import cz.cvut.kbss.jopa.environment.Vocabulary;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.exceptions.NoResultException;
import cz.cvut.kbss.jopa.exceptions.NoUniqueResultException;
import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.IdentifiableEntityType;
import cz.cvut.kbss.jopa.model.metamodel.PropertiesSpecification;
import cz.cvut.kbss.jopa.model.query.Query;
import cz.cvut.kbss.jopa.model.query.TypedQuery;
import cz.cvut.kbss.jopa.query.QueryHints;
import cz.cvut.kbss.jopa.query.QueryParameter;
import cz.cvut.kbss.jopa.query.parameter.ParameterValueFactory;
import cz.cvut.kbss.jopa.vocabulary.RDF;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Stream;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyCollection;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class TypedQueryImplTest extends QueryTestBase {

    private static final String ASK_BOOLEAN_QUERY =
            "ASK { ?x a <https://onto.fel.cvut.cz/ontologies/jopa/entities#OWLClassA> . }";

    @Override
    TypedQueryImpl<?> createQuery(String query, Class<?> resultType) {
        final TypedQueryImpl<?> q = queryFactory.createNativeQuery(query, resultType);
        q.setEnsureOpenProcedure(ensureOpenProcedure);
        return q;
    }

    @Override
    TypedQueryImpl<?> createQuery(String query) {
        return createQuery(query, Void.class);
    }

    @BeforeEach
    void setUp() throws Exception {
        super.setUp();
        when(uowMock.isEntityType(OWLClassA.class)).thenReturn(true);
    }

    @Test
    void getResultListWithEntityTypeReturnsEntities() throws Exception {
        final TypedQuery<OWLClassA> query = create(SELECT_QUERY, OWLClassA.class);
        final List<String> uris = initDataForQuery(5);
        final List<OWLClassA> res = query.getResultList();
        verifyResults(uris, res, 5);
    }

    private <T> TypedQueryImpl<T> create(String query, Class<T> type) {
        final TypedQueryImpl<T> q = queryFactory.createNativeQuery(query, type);
        q.setEnsureOpenProcedure(ensureOpenProcedure);
        return q;
    }

    private List<String> initDataForQuery(int count) throws Exception {
        final List<String> uris = new ArrayList<>(count);
        final List<Boolean> hasNext = new ArrayList<>(count);
        for (int i = 0; i < count; i++) {
            final String u = "http://uri" + i;
            uris.add(u);
            when(uowMock.readObject(eq(OWLClassA.class), eq(URI.create(u)), any()))
                    .thenReturn(new OWLClassA(URI.create(u)));
            hasNext.add(true);
        }
        hasNext.add(false);
        when(resultRow.getString(0))
                .thenReturn(uris.get(0), uris.subList(1, uris.size()).toArray(new String[count]));
        when(resultSetIterator.hasNext())
                .thenReturn(hasNext.get(0), hasNext.subList(1, hasNext.size()).toArray(new Boolean[count]));
        when(resultRow.isBound(0)).thenReturn(true);
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
    void getSingleResultWithEntityTypeReturnsCorrectResult() throws Exception {
        final TypedQuery<OWLClassA> query = create(SELECT_QUERY, OWLClassA.class);
        final List<String> uris = initDataForQuery(1);
        final OWLClassA res = query.getSingleResult();
        assertNotNull(res);
        assertEquals(uris.get(0), res.getUri().toString());
    }

    @Test
    void setMaxResultsExecutesQueryWithSpecifiedLimit() throws Exception {
        final TypedQuery<OWLClassA> query = create(SELECT_QUERY, OWLClassA.class);
        final int expectedCount = 5;
        query.setMaxResults(expectedCount).getResultList();
        final ArgumentCaptor<String> captor = ArgumentCaptor.forClass(String.class);
        verify(statementMock).executeQuery(captor.capture());
        assertThat(captor.getValue(), containsString("LIMIT " + expectedCount));
    }

    @Test
    void throwsExceptionWhenNegativeIsUsedForSetMaxResults() {
        final TypedQuery<OWLClassA> query = create(SELECT_QUERY, OWLClassA.class);
        assertThrows(IllegalArgumentException.class, () -> query.setMaxResults(-1).getResultList());
    }

    @Test
    void throwsNoResultExceptionWhenThereIsNoResultForGetSingle() {
        final TypedQuery<OWLClassA> query = create(SELECT_QUERY, OWLClassA.class);
        assertThrows(NoResultException.class, query::getSingleResult);
    }

    @Test
    void throwsNoSingleResultExceptionWhenThereAreMultipleResultsForGetSingle() throws Exception {
        final TypedQuery<OWLClassA> query = create(SELECT_QUERY, OWLClassA.class);
        initDataForQuery(5);
        assertThrows(NoUniqueResultException.class, query::getSingleResult);
    }

    @Test
    void askQueryReturnsSingleBoolean() throws Exception {
        final TypedQuery<Boolean> query = create(ASK_BOOLEAN_QUERY, Boolean.class);
        initAskQueryData(true);
        final Boolean result = query.getSingleResult();
        assertNotNull(result);
        assertTrue(result);
        verify(uowMock, never()).readObject(eq(Boolean.class), any(), any(Descriptor.class));
    }

    private void initAskQueryData(boolean result) throws Exception {
        when(resultSetIterator.hasNext()).thenReturn(true, false);
        when(resultRow.getObject(0, Boolean.class)).thenReturn(result);
    }

    @Test
    void executeUpdateRunsUpdateOnConnection() throws Exception {
        final String update = """
                DELETE { ?inst ?property ?origValue . }
                INSERT { ?inst ?property ?newValue . }
                WHERE { ?inst ?property ?origValue . }""";
        final TypedQueryImpl<?> q = createQuery(update);
        q.executeUpdate();
        verify(statementMock).executeUpdate(update);
    }

    @Test
    void executeUpdateThrowsPersistenceExceptionWhenOntoDriverExceptionIsThrown() throws Exception {
        doThrow(new OntoDriverException()).when(statementMock).executeUpdate(UPDATE_QUERY);
        final Query q = create(UPDATE_QUERY, Void.class);
        final OWLPersistenceException result = assertThrows(OWLPersistenceException.class, q::executeUpdate);
        assertEquals("Exception caught when evaluating query " + UPDATE_QUERY, result.getMessage());
    }

    @Test
    void getResultListSkipsValuesWhichCannotBeLoadedAsEntities() throws Exception {
        when(resultSetIterator.hasNext()).thenReturn(true, true, false);
        final List<String> uris = Arrays.asList(Generators.createIndividualIdentifier().toString(),
                Generators.createIndividualIdentifier().toString());
        when(resultRow.isBound(0)).thenReturn(true);
        when(resultRow.getString(0)).thenReturn(uris.get(0), uris.get(1));
        when(uowMock.readObject(eq(OWLClassA.class), eq(URI.create(uris.get(0))), any(Descriptor.class)))
                .thenReturn(new OWLClassA(URI.create(uris.get(0))));

        final TypedQuery<OWLClassA> q = create(SELECT_QUERY, OWLClassA.class);
        final List<OWLClassA> result = q.getResultList();
        assertEquals(1, result.size());
    }

    @Test
    void exceptionInExecuteUpdateInvokesRollbackMarker() throws Exception {
        doThrow(new OntoDriverException()).when(statementMock).executeUpdate(UPDATE_QUERY);
        final TypedQueryImpl<Void> q = create(UPDATE_QUERY, Void.class);
        runAndVerifyHandlerInvocation(q, q::executeUpdate);
    }

    private <T> void runAndVerifyHandlerInvocation(TypedQueryImpl<T> query, Runnable method) {
        query.setRollbackOnlyMarker(handler);
        try {
            method.run();
        } catch (RuntimeException e) {
            // Swallow the exception
        }
        verify(handler).run();
    }

    @Test
    void runtimeExceptionInExecuteUpdateInvokesRollbackMarker() throws Exception {
        doThrow(OWLPersistenceException.class).when(statementMock).executeUpdate(UPDATE_QUERY);
        final TypedQueryImpl<Void> q = create(UPDATE_QUERY, Void.class);
        runAndVerifyHandlerInvocation(q, q::executeUpdate);
    }

    @Test
    void exceptionInGetResultListInvokesRollbackMarker() throws Exception {
        doThrow(OntoDriverException.class).when(statementMock).executeQuery(SELECT_QUERY);
        final TypedQueryImpl<OWLClassA> q = create(SELECT_QUERY, OWLClassA.class);
        runAndVerifyHandlerInvocation(q, q::getResultList);
    }

    @Test
    void runtimeExceptionInGetResultListInvokesRollbackMarker() throws Exception {
        doThrow(OWLPersistenceException.class).when(statementMock).executeQuery(SELECT_QUERY);
        final TypedQueryImpl<OWLClassA> q = create(SELECT_QUERY, OWLClassA.class);
        runAndVerifyHandlerInvocation(q, q::getResultList);
    }

    @Test
    void exceptionInGetSingleResultInvokesRollbackMarker() throws Exception {
        doThrow(OntoDriverException.class).when(statementMock).executeQuery(SELECT_QUERY);
        final TypedQueryImpl<OWLClassA> q = create(SELECT_QUERY, OWLClassA.class);
        runAndVerifyHandlerInvocation(q, q::getSingleResult);
    }

    @Test
    void runtimeExceptionInGetSingleResultInvokesRollbackMarker() throws Exception {
        doThrow(OWLPersistenceException.class).when(statementMock).executeQuery(SELECT_QUERY);
        final TypedQueryImpl<OWLClassA> q = create(SELECT_QUERY, OWLClassA.class);
        runAndVerifyHandlerInvocation(q, q::getSingleResult);
    }

    @Test
    void exceptionInSetMaxResultsInvokesRollbackMarker() {
        final TypedQueryImpl<OWLClassA> q = queryWithRollbackMarker(SELECT_QUERY, OWLClassA.class);
        try {
            q.setMaxResults(-1);
        } catch (RuntimeException e) {
            // Swallow the exception
        }
        verify(handler).run();
    }

    private <T> TypedQueryImpl<T> queryWithRollbackMarker(String query, Class<T> cls) {
        final TypedQueryImpl<T> q = create(query, cls);
        q.setRollbackOnlyMarker(handler);
        return q;
    }

    @Test
    void exceptionInSetParameterByPositionInvokesRollbackMarker() {
        final TypedQueryImpl<OWLClassA> q = queryWithRollbackMarker(SELECT_QUERY, OWLClassA.class);
        try {
            q.setParameter(117, 117);
        } catch (RuntimeException e) {
            // Swallow the exception
        }
        verify(handler).run();
    }

    @Test
    void exceptionInSetStringParameterByPositionInvokesRollbackMarker() {
        final TypedQueryImpl<OWLClassA> q = queryWithRollbackMarker(SELECT_QUERY, OWLClassA.class);
        try {
            q.setParameter(117, "A", "en");
        } catch (RuntimeException e) {
            // Swallow the exception
        }
        verify(handler).run();
    }

    @Test
    void exceptionInSetParameterByNameInvokesRollbackMarker() {
        final TypedQueryImpl<OWLClassA> q = queryWithRollbackMarker(SELECT_QUERY, OWLClassA.class);
        try {
            q.setParameter("a", 117);
        } catch (RuntimeException e) {
            // Swallow the exception
        }
        verify(handler).run();
    }

    @Test
    void exceptionInSetStringParameterByNameInvokesRollbackMarker() {
        final TypedQueryImpl<OWLClassA> q = queryWithRollbackMarker(SELECT_QUERY, OWLClassA.class);
        try {
            q.setParameter("a", "A", "en");
        } catch (RuntimeException e) {
            // Swallow the exception
        }
        verify(handler).run();
    }

    @Test
    void exceptionInSetParameterByParameterInvokesRollbackMarker() {
        final TypedQueryImpl<OWLClassA> q = queryWithRollbackMarker(SELECT_QUERY, OWLClassA.class);
        try {
            q.setParameter(new QueryParameter<>(117, mock(ParameterValueFactory.class)), 117);
        } catch (RuntimeException e) {
            // Swallow the exception
        }
        verify(handler).run();
    }

    @Test
    void exceptionInSetStringParameterByParameterInvokesRollbackMarker() {
        final TypedQueryImpl<OWLClassA> q = queryWithRollbackMarker(SELECT_QUERY, OWLClassA.class);
        try {
            q.setParameter(new QueryParameter<>(117, mock(ParameterValueFactory.class)), "A", "en");
        } catch (RuntimeException e) {
            // Swallow the exception
        }
        verify(handler).run();
    }

    @Test
    void setDescriptorPassesDescriptorToInstanceLoading() throws Exception {
        final TypedQuery<OWLClassA> query = create(SELECT_QUERY, OWLClassA.class);
        final int count = 10;
        final List<String> uris = initDataForQuery(count);
        final Descriptor descriptor = new EntityDescriptor(URI.create("http://contextOne"));
        query.setDescriptor(descriptor).getResultList();
        for (String uri : uris) {
            verify(uowMock).readObject(OWLClassA.class, URI.create(uri), descriptor);
        }
    }

    @Test
    void setFirstResultExecutesQueryWithSpecifiedOffset() throws Exception {
        final TypedQuery<OWLClassA> q = create(SELECT_QUERY, OWLClassA.class);
        initDataForQuery(5);
        final int position = 3;
        q.setFirstResult(position).getResultList();
        final ArgumentCaptor<String> captor = ArgumentCaptor.forClass(String.class);
        verify(statementMock).executeQuery(captor.capture());
        assertThat(captor.getValue(), containsString("OFFSET " + position));
    }

    @Test
    void noUniqueResultExceptionInGetSingleResultDoesNotCauseTransactionRollback() throws Exception {
        final TypedQueryImpl<OWLClassA> query = create(SELECT_QUERY, OWLClassA.class);
        initDataForQuery(5);
        query.setRollbackOnlyMarker(handler);
        assertThrows(NoUniqueResultException.class, query::getSingleResult);
        verify(handler, never()).run();
    }

    @Test
    void noResultExceptionInGetSingleResultDoesNotCauseTransactionRollback() {
        final TypedQueryImpl<OWLClassA> query = create(SELECT_QUERY, OWLClassA.class);
        query.setRollbackOnlyMarker(handler);
        assertThrows(NoResultException.class, query::getSingleResult);
        verify(handler, never()).run();
    }

    @Test
    void getResultStreamRetrievesResultStreamFromUnderlyingResultSet() throws Exception {
        final List<String> uris = Arrays.asList(Generators.createIndividualIdentifier().toString(),
                Generators.createIndividualIdentifier().toString());
        when(resultSetMock.isOpen()).thenReturn(true);
        when(resultSetMock.hasNext()).thenReturn(true, true, false);
        when(resultSetMock.isBound(0)).thenReturn(true);
        when(resultSetMock.getString(0)).thenReturn(uris.get(0), uris.get(1));
        when(uowMock.readObject(eq(OWLClassA.class), eq(URI.create(uris.get(0))), any(Descriptor.class)))
                .thenReturn(new OWLClassA(URI.create(uris.get(0))));
        when(uowMock.readObject(eq(OWLClassA.class), eq(URI.create(uris.get(1))), any(Descriptor.class)))
                .thenReturn(new OWLClassA(URI.create(uris.get(1))));
        final TypedQuery<OWLClassA> sut = create(SELECT_QUERY, OWLClassA.class);
        final Stream<OWLClassA> result = sut.getResultStream();
        final List<OWLClassA> asList = result.toList();
        assertEquals(uris.size(), asList.size());
        assertTrue(uris.containsAll(asList.stream().map(a -> a.getUri().toString()).toList()));
    }

    @Test
    void getResultStreamClosesStatementWhenStreamIsProcessed() throws Exception {
        final List<String> uris = Collections.singletonList(Generators.createIndividualIdentifier().toString());
        when(resultSetMock.isOpen()).thenReturn(true);
        when(resultSetMock.hasNext()).thenReturn(true, false);
        when(resultSetMock.isBound(0)).thenReturn(true);
        when(resultSetMock.getString(0)).thenReturn(uris.get(0));
        when(uowMock.readObject(eq(OWLClassA.class), eq(URI.create(uris.get(0))), any(Descriptor.class)))
                .thenReturn(new OWLClassA(URI.create(uris.get(0))));
        final TypedQuery<OWLClassA> sut = create(SELECT_QUERY, OWLClassA.class);
        sut.getResultStream().forEach(a -> assertTrue(uris.contains(a.getUri().toString())));
        verify(statementMock).close();
    }

    @Test
    void getResultStreamClosesStatementWhenStreamProcessingThrowsException() throws Exception {
        final List<String> uris = Collections.singletonList(Generators.createIndividualIdentifier().toString());
        when(resultSetMock.isOpen()).thenReturn(true);
        when(resultSetMock.hasNext()).thenReturn(true, false);
        when(resultSetMock.isBound(0)).thenReturn(true);
        when(resultSetMock.getString(0)).thenReturn(uris.get(0));
        when(uowMock.readObject(eq(OWLClassA.class), eq(URI.create(uris.get(0))), any(Descriptor.class)))
                .thenThrow(OWLPersistenceException.class);
        final TypedQuery<OWLClassA> sut = create(SELECT_QUERY, OWLClassA.class);
        try {
            assertThrows(OWLPersistenceException.class,
                    () -> sut.getResultStream().forEach(a -> assertTrue(uris.contains(a.getUri().toString()))));
        } finally {
            verify(statementMock).close();
        }
    }

    @Test
    void optimizeEntityResultLoadingHintAppliesEntityLoadingOptimizer() throws Exception {
        final OWLClassB entity = new OWLClassB(Generators.createIndividualIdentifier());
        final TypedQuery<OWLClassB> query = create(SELECT_QUERY, OWLClassB.class);
        query.setHint(QueryHints.ENABLE_ENTITY_LOADING_OPTIMIZER, true);
        when(resultSetMock.iterator()).thenReturn(resultSetIterator);
        when(resultSetIterator.hasNext()).thenReturn(true, false);
        when(resultSetIterator.next()).thenReturn(resultRow);
        when(resultRow.getObject(0, URI.class)).thenReturn(entity.getUri());
        when(resultRow.getObject(1, URI.class)).thenReturn(URI.create(RDF.TYPE));
        when(resultRow.getObject(2)).thenReturn(URI.create(Vocabulary.c_OwlClassB));
        when(resultRow.getColumnCount()).thenReturn(3);
        when(uowMock.readObjectFromAxioms(eq(OWLClassB.class), anyCollection(), any())).thenReturn(entity);
        final MetamodelImpl metamodel = mock(MetamodelImpl.class);
        when(uowMock.getMetamodel()).thenReturn(metamodel);
        when(uowMock.isEntityType(OWLClassB.class)).thenReturn(true);
        final IdentifiableEntityType<OWLClassB> et = mock(IdentifiableEntityType.class);
        when(et.getProperties()).thenReturn(mock(PropertiesSpecification.class));
        when(metamodel.entity(OWLClassB.class)).thenReturn(et);

        final List<OWLClassB> res = query.getResultList();
        assertEquals(List.of(entity), res);
        verify(uowMock, never()).readObject(eq(OWLClassA.class), eq(entity.getUri()), any());
    }
}

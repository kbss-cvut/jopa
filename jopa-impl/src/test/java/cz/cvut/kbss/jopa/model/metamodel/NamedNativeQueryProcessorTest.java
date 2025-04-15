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
package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.model.annotations.NamedNativeQueries;
import cz.cvut.kbss.jopa.model.annotations.NamedNativeQuery;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.query.NamedQueryManager;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Field;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

class NamedNativeQueryProcessorTest {

    private static final String SELECT_QUERY = "SELECT ?x ?y ?z WHERE { ?x ?y ?z .}";
    private static final String SELECT_NAME = "selectQuery";
    private static final String ASK_QUERY = "ASK WHERE { ?x a ?type . }";
    private static final String ASK_NAME = "askQuery";

    private NamedNativeQueryProcessor processor;

    @BeforeEach
    void setUp() {
        this.processor = new NamedNativeQueryProcessor();
    }

    private NamedQueryManager queryManager() {
        return processor.getQueryManager();
    }

    @Test
    void processesSingleQueryDeclaredOnClass() {
        processor.processClass(SingleQuery.class);
        final String query = queryManager().getQuery(SingleQuery.class.getSimpleName() + "." + SELECT_NAME);
        assertNotNull(query);
        assertEquals(SELECT_QUERY, query);
    }


    @NamedNativeQuery(name = "SingleQuery." + SELECT_NAME, query = SELECT_QUERY)
    @OWLClass(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/entities#SingleQuery")
    private static class SingleQuery {
    }

    @Test
    void processesNamedNativeQueriesDeclaredOnClass() {
        processor.processClass(MultipleQueries.class);
        final String select = queryManager().getQuery(MultipleQueries.class.getSimpleName() + "." + SELECT_NAME);
        assertEquals(SELECT_QUERY, select);
        final String ask = queryManager().getQuery(MultipleQueries.class.getSimpleName() + "." + ASK_NAME);
        assertEquals(ASK_QUERY, ask);
    }

    @NamedNativeQueries({
            @NamedNativeQuery(name = "MultipleQueries." + SELECT_NAME, query = SELECT_QUERY),
            @NamedNativeQuery(name = "MultipleQueries." + ASK_NAME, query = ASK_QUERY)})
    @OWLClass(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/entities#MultipleQueries")
    private static class MultipleQueries {
    }

    @Test
    void doesNothingForClassWithoutNamedNativeQueries() throws Exception {
        processor.processClass(OWLClassA.class);
        final Field queriesField = NamedQueryManager.class.getDeclaredField("queryMap");
        queriesField.setAccessible(true);
        final Map<?, ?> queryMap = (Map<?, ?>) queriesField.get(queryManager());
        assertTrue(queryMap.isEmpty());
    }

    @Test
    void processesClassWithCombinationOfNamedNativeQueriesAndNamedNativeQuery() {
        processor.processClass(QueryCombination.class);
        final String select = queryManager().getQuery(QueryCombination.class.getSimpleName() + "." + SELECT_NAME);
        assertEquals(SELECT_QUERY, select);
        final String ask = queryManager().getQuery(QueryCombination.class.getSimpleName() + "." + ASK_NAME);
        assertEquals(ASK_QUERY, ask);
    }

    @NamedNativeQueries({
            @NamedNativeQuery(name = "QueryCombination." + SELECT_NAME, query = SELECT_QUERY),
    })
    @NamedNativeQuery(name = "QueryCombination." + ASK_NAME, query = ASK_QUERY)
    @OWLClass(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/entities#QueryCombination")
    private static class QueryCombination {
    }
}

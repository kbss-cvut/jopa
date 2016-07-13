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
package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.model.annotations.MappedSuperclass;
import cz.cvut.kbss.jopa.model.annotations.NamedNativeQueries;
import cz.cvut.kbss.jopa.model.annotations.NamedNativeQuery;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.query.NamedQueryManager;
import org.junit.Before;
import org.junit.Test;

import java.lang.reflect.Field;
import java.util.Map;

import static org.junit.Assert.*;

public class NamedNativeQueryProcessorTest {

    private static final String SELECT_QUERY = "SELECT ?x ?y ?z WHERE { ?x ?y ?z .}";
    private static final String SELECT_NAME = "selectQuery";
    private static final String ASK_QUERY = "ASK WHERE { ?x a ?type . }";
    private static final String ASK_NAME = "askQuery";

    private NamedQueryManager queryManager;

    private NamedNativeQueryProcessor processor;

    @Before
    public void setUp() {
        this.queryManager = new NamedQueryManager();
        this.processor = new NamedNativeQueryProcessor(queryManager);
    }

    @Test
    public void processesSingleQueryDeclaredOnClass() {
        processor.processClass(SingleQuery.class);
        final String query = queryManager.getQuery(SingleQuery.class.getSimpleName() + "." + SELECT_NAME);
        assertNotNull(query);
        assertEquals(SELECT_QUERY, query);
    }


    @NamedNativeQuery(name = "SingleQuery." + SELECT_NAME, query = SELECT_QUERY)
    @OWLClass(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/entities#SingleQuery")
    private static class SingleQuery {
    }

    @Test
    public void processesNamedNativeQueriesDeclaredOnClass() {
        processor.processClass(MultipleQueries.class);
        final String select = queryManager.getQuery(MultipleQueries.class.getSimpleName() + "." + SELECT_NAME);
        assertEquals(SELECT_QUERY, select);
        final String ask = queryManager.getQuery(MultipleQueries.class.getSimpleName() + "." + ASK_NAME);
        assertEquals(ASK_QUERY, ask);
    }

    @NamedNativeQueries({
            @NamedNativeQuery(name = "MultipleQueries." + SELECT_NAME, query = SELECT_QUERY),
            @NamedNativeQuery(name = "MultipleQueries." + ASK_NAME, query = ASK_QUERY)})
    @OWLClass(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/entities#MultipleQueries")
    private static class MultipleQueries {
    }

    @Test
    public void doesNothingForClassWithoutNamedNativeQueries() throws Exception {
        processor.processClass(OWLClassA.class);
        final Field queriesField = NamedQueryManager.class.getDeclaredField("queryMap");
        queriesField.setAccessible(true);
        final Map<?, ?> queryMap = (Map<?, ?>) queriesField.get(queryManager);
        assertTrue(queryMap.isEmpty());
    }

    @Test
    public void processesClassWithCombinationOfNamedNativeQueriesAndNamedNativeQuery() {
        processor.processClass(QueryCombination.class);
        final String select = queryManager.getQuery(QueryCombination.class.getSimpleName() + "." + SELECT_NAME);
        assertEquals(SELECT_QUERY, select);
        final String ask = queryManager.getQuery(QueryCombination.class.getSimpleName() + "." + ASK_NAME);
        assertEquals(ASK_QUERY, ask);
    }

    @NamedNativeQueries({
            @NamedNativeQuery(name = "QueryCombination." + SELECT_NAME, query = SELECT_QUERY),
    })
    @NamedNativeQuery(name = "QueryCombination." + ASK_NAME, query = ASK_QUERY)
    @OWLClass(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/entities#QueryCombination")
    private static class QueryCombination {
    }

    @Test
    public void processClassDiscoversQueriesDeclaredInMappedSuperclass() {
        processor.processClass(WithMappedSuperclass.class);
        final String select = queryManager.getQuery(Superclass.class.getSimpleName() + "." + SELECT_NAME);
        assertEquals(SELECT_QUERY, select);
        final String ask = queryManager.getQuery(WithMappedSuperclass.class.getSimpleName() + "." + ASK_NAME);
        assertEquals(ASK_QUERY, ask);
    }

    @MappedSuperclass
    @NamedNativeQuery(name = "Superclass." + SELECT_NAME, query = SELECT_QUERY)
    private static abstract class Superclass {
    }

    @NamedNativeQuery(name = "WithMappedSuperclass." + ASK_NAME, query = ASK_QUERY)
    @OWLClass(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/entities#WithMappedSuperclass")
    private static class WithMappedSuperclass extends Superclass {
    }
}

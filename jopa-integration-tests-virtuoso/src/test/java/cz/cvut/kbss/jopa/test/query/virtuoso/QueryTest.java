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
package cz.cvut.kbss.jopa.test.query.virtuoso;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.Vocabulary;
import cz.cvut.kbss.jopa.test.environment.Generators;
import cz.cvut.kbss.jopa.test.environment.VirtuosoDataAccessor;
import cz.cvut.kbss.jopa.test.environment.VirtuosoPersistenceFactory;
import cz.cvut.kbss.jopa.test.query.QueryTestEnvironment;
import cz.cvut.kbss.jopa.test.query.runner.QueryRunner;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.EnabledIfSystemProperty;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.net.URI;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

@EnabledIfSystemProperty(named = "virtuoso.host", matches = ".+")
@EnabledIfSystemProperty(named = "virtuoso.port", matches = ".+")
public class QueryTest extends QueryRunner {

    private static final Logger LOG = LoggerFactory.getLogger(QueryTest.class);

    private static EntityManager em;

    QueryTest() {
        super(LOG, new VirtuosoDataAccessor());
    }

    @BeforeEach
    void setUp() {
        final VirtuosoPersistenceFactory persistenceFactory = new VirtuosoPersistenceFactory();
        em = persistenceFactory.getEntityManager("SPARQLQueryTests", false, Map.of());
        QueryTestEnvironment.generateTestData(em);
        em.clear();
        em.getEntityManagerFactory().getCache().evictAll();
    }

    @AfterEach
    void tearDown() {
        VirtuosoDataAccessor.clearRepository(em);
        em.close();
        em.getEntityManagerFactory().close();
    }

    @Override
    protected EntityManager getEntityManager() {
        return em;
    }

    @Test
    @Override
    public void executeUpdateRunsUpdateOnRepository() {
        final EntityManager em = getEntityManager();
        final OWLClassA instance = QueryTestEnvironment.getData(OWLClassA.class).get(0);
        final String newValue = "UpdatedValue";
        final String update = "DELETE { GRAPH ?g { ?inst ?property ?origValue . } }" +
                "INSERT { GRAPH <http://test> { ?inst ?property ?newValue . } } WHERE {" +
                " GRAPH ?g { ?inst ?property ?origValue . } }";
        em.createNativeQuery(update).setParameter("inst", instance.getUri()).setParameter("property", URI.create(
                Vocabulary.P_A_STRING_ATTRIBUTE)).setParameter("newValue", newValue, "en").executeUpdate();

        final OWLClassA result = em.find(OWLClassA.class, instance.getUri());
        assertEquals(newValue, result.getStringAttribute());
    }

    @Test
    @Override
    public void executeUpdateRunsDeleteOnRepository() {
        final EntityManager em = getEntityManager();
        final OWLClassA instance = QueryTestEnvironment.getData(OWLClassA.class).get(0);
        assertNotNull(instance.getStringAttribute());
        final String update = "DELETE { GRAPH ?g { ?inst ?property ?origValue . } } WHERE { GRAPH ?g { ?inst ?property ?origValue . } }";
        em.createNativeQuery(update).setParameter("inst", instance.getUri())
          .setParameter("property", URI.create(Vocabulary.P_A_STRING_ATTRIBUTE)).executeUpdate();

        final OWLClassA result = em.find(OWLClassA.class, instance.getUri());
        assertNull(result.getStringAttribute());
    }

    @Test
    @Override
    public void executeUpdateRunsInsertOnRepository() {
        final EntityManager em = getEntityManager();
        final URI newType = Generators.generateUri();
        final OWLClassA instance = QueryTestEnvironment.getData(OWLClassA.class).get(0);
        final String update = "INSERT DATA { GRAPH <http://test> { ?inst a ?newType . } }";
        em.createNativeQuery(update).setParameter("inst", instance.getUri())
          .setParameter("newType", newType).executeUpdate();

        final OWLClassA result = em.find(OWLClassA.class, instance.getUri());
        assertTrue(result.getTypes().contains(newType.toString()));
    }
}

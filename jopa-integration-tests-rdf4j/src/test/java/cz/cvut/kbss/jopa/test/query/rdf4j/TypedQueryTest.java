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
package cz.cvut.kbss.jopa.test.query.rdf4j;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.model.JOPAExperimentalProperties;
import cz.cvut.kbss.jopa.test.environment.Rdf4jDataAccessor;
import cz.cvut.kbss.jopa.test.environment.Rdf4jPersistenceFactory;
import cz.cvut.kbss.jopa.test.query.QueryTestEnvironment;
import cz.cvut.kbss.jopa.test.query.runner.TypedQueryRunner;
import cz.cvut.kbss.ontodriver.rdf4j.config.Rdf4jOntoDriverProperties;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Map;

public class TypedQueryTest extends TypedQueryRunner {

    private static final Logger LOG = LoggerFactory.getLogger(TypedQueryTest.class);

    private static EntityManager em;

    TypedQueryTest() {
        super(LOG, new Rdf4jDataAccessor());
    }

    @BeforeAll
    static void setUpBeforeClass() {
        final Rdf4jPersistenceFactory persistenceFactory = new Rdf4jPersistenceFactory();
        em = persistenceFactory.getEntityManager("SPARQLTypedQueryTests", false,
                Map.of(Rdf4jOntoDriverProperties.USE_INFERENCE, "true",
                        JOPAExperimentalProperties.QUERY_ENABLE_ENTITY_LOADING_OPTIMIZER, "true"));
        QueryTestEnvironment.generateTestData(em);
        em.clear();
        em.getEntityManagerFactory().getCache().evictAll();
    }

    @BeforeEach
    void setUp() {
        em.clear();
    }

    @AfterAll
    static void tearDownAfterClass() {
        em.close();
        em.getEntityManagerFactory().close();
    }

    @Override
    protected EntityManager getEntityManager() {
        return em;
    }

    @Disabled
    @Test
    @Override
    public void askQueryAgainstTransactionalOntologyContainsUncommittedChangesAsWell() {
        // RDF4J does not support queries against transactional snapshot because it does not use it
    }
}

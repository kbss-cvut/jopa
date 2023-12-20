/*
 * JOPA
 * Copyright (C) 2023 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.test.query.owlapi;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.test.environment.OwlapiDataAccessor;
import cz.cvut.kbss.jopa.test.environment.OwlapiPersistenceFactory;
import cz.cvut.kbss.jopa.test.query.QueryTestEnvironment;
import cz.cvut.kbss.jopa.test.query.runner.QueryRunner;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Collections;

public class QueryTest extends QueryRunner {

    private static final Logger LOG = LoggerFactory.getLogger(QueryTest.class);

    private static EntityManager em;

    QueryTest() {
        super(LOG, new OwlapiDataAccessor());
    }

    @BeforeAll
    static void setUpBeforeClass() {
        final OwlapiPersistenceFactory persistenceFactory = new OwlapiPersistenceFactory();
        em = persistenceFactory.getEntityManager("SPARQLQueryTests", false, Collections.emptyMap());
        QueryTestEnvironment.generateTestData(em);
        em.clear();
        em.getEntityManagerFactory().getCache().evictAll();
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
    @Override
    public void selectWithOptionalReturnsNullInUnfilledColumns() {
        // OWL2Query does not support OPTIONAL pattern
    }

    @Disabled
    @Override
    public void queryWithEntityMappingLoadsReferencedEntityAndInheritedAttributes() {
        // OWL2Query does not return any results for the test query
    }

    @Disabled
    @Test
    public void executeUpdateRunsUpdateOnRepository() {
        // No updates in OWL2Query
    }

    @Disabled
    @Test
    public void executeUpdateRunsDeleteOnRepository() {
        // No updates in OWL2Query
    }

    @Disabled
    @Test
    public void executeUpdateRunsInsertOnRepository() {
        // No updates in OWL2Query
    }

    @Disabled
    @Override
    public void setParameterSupportsLangStringValue() {
        // OWL2Query does not support IN operator
    }
}

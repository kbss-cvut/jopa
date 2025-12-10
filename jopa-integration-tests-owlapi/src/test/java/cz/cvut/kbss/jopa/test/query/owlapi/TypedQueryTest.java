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
package cz.cvut.kbss.jopa.test.query.owlapi;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.environment.OwlapiDataAccessor;
import cz.cvut.kbss.jopa.test.environment.OwlapiPersistenceFactory;
import cz.cvut.kbss.jopa.test.query.QueryTestEnvironment;
import cz.cvut.kbss.jopa.test.query.runner.TypedQueryRunner;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class TypedQueryTest extends TypedQueryRunner {

    private static final Logger LOG = LoggerFactory.getLogger(TypedQueryTest.class);

    private static EntityManager em;

    TypedQueryTest() {
        super(LOG, new OwlapiDataAccessor());
    }

    @Override
    protected EntityManager getEntityManager() {
        return em;
    }

    @BeforeAll
    static void setUpBeforeClass() {
        final OwlapiPersistenceFactory persistenceFactory = new OwlapiPersistenceFactory();
        em = persistenceFactory.getEntityManager("SPARQLTypedQueryTests", false, Collections.emptyMap());
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
    @Test
    public void setFirstResultCanBeUsedToOffsetFirstQueryResult() {
        final List<OWLClassA> expected = QueryTestEnvironment.getData(OWLClassA.class);
        final int offset = expected.size() / 2;
        final List<OWLClassA> result = getEntityManager().createNamedQuery("OWLClassA.findAll", OWLClassA.class)
                                                         .setFirstResult(offset).getResultList();
        assertEquals(expected.size() - offset, result.size());
        // OWL2Query does not support ORDER BY, so we can't use it to verify the offset
    }

    @Disabled
    @Test
    @Override
    protected void querySupportsCollectionParameters() {
        // OWL2Query does not support FILTER (?x IN ...) in queries
    }

    @Disabled
    @Test
    @Override
    protected void querySupportsSelectionByDate() {
        // OWL2Query does not support filter by date and delete (used in cleanup)
    }

    @Disabled
    @Test
    @Override
    public void setUntypedParameterAllowSpecifyingFilterValue() {
        // OWL2Query does not support complex filters
    }

    @Disabled
    @Test
    @Override
    public void querySupportsOptimizedEntityLoadingOfClassWithUnmappedProperties() {
        // OWL2Query did not correct results
    }

    @Disabled
    @Test
    @Override
    public void querySupportsOptimizedEntityLoading() {
        // OWL2Query does not support OPTIONAL
    }

    @Disabled
    @Test
    @Override
    public void optimizedEntityLoadingWorksForEntityClassesWithoutTypesField() {
        super.optimizedEntityLoadingWorksForEntityClassesWithoutTypesField();
    }
}

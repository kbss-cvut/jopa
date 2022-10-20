/**
 * Copyright (C) 2022 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.test.query.owlapi;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.environment.OwlapiDataAccessor;
import cz.cvut.kbss.jopa.test.environment.OwlapiPersistenceFactory;
import cz.cvut.kbss.jopa.test.query.QueryTestEnvironment;
import cz.cvut.kbss.jopa.test.query.runner.TypedQueryRunner;
import org.junit.jupiter.api.*;
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

    @Disabled
    @Test
    @Override
    public void usingUntypedQueryAllowsToSpecifyLimitInQuery() {
        // OWL2Query does not support LIMIT in queries
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
}

/**
 * Copyright (C) 2019 Czech Technical University in Prague
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
import cz.cvut.kbss.jopa.test.environment.OwlapiPersistenceFactory;
import cz.cvut.kbss.jopa.test.query.QueryTestEnvironment;
import cz.cvut.kbss.jopa.test.query.runner.QueryRunner;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Collections;

public class QueryTest extends QueryRunner {

    private static final Logger LOG = LoggerFactory.getLogger(QueryTest.class);

    private static EntityManager em;

    public QueryTest() {
        super(LOG);
    }

    @BeforeClass
    public static void setUpBeforeClass() {
        final OwlapiPersistenceFactory persistenceFactory = new OwlapiPersistenceFactory();
        em = persistenceFactory.getEntityManager("SPARQLQueryTests", false, Collections.emptyMap());
        QueryTestEnvironment.generateTestData(em);
        em.clear();
        em.getEntityManagerFactory().getCache().evictAll();
    }

    @AfterClass
    public static void tearDownAfterClass() {
        em.close();
        em.getEntityManagerFactory().close();
    }

    @Override
    protected EntityManager getEntityManager() {
        return em;
    }

    @Ignore
    @Override
    public void selectWithOptionalReturnsNullInUnfilledColumns() {
        // OWL2Query does not support OPTIONAL pattern
    }

    @Ignore
    @Override
    public void queryWithEntityMappingLoadsReferencedEntityAndInheritedAttributes() {
        // OWL2Query does not return any results for the test query
    }

    @Ignore
    @Test
    public void executeUpdateRunsUpdateOnRepository() {
        // No updates in OWL2Query
    }

    @Ignore
    @Test
    public void executeUpdateRunsDeleteOnRepository() {
        // No updates in OWL2Query
    }

    @Ignore
    @Test
    public void executeUpdateRunsInsertOnRepository() {
        // No updates in OWL2Query
    }
}

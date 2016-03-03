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
package cz.cvut.kbss.jopa.test.query;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.test.environment.SesamePersistenceFactory;
import cz.cvut.kbss.jopa.test.query.runner.QueryRunner;
import cz.cvut.kbss.ontodriver.sesame.config.SesameOntoDriverProperties;
import org.junit.AfterClass;
import org.junit.BeforeClass;

import java.util.Collections;
import java.util.logging.Logger;

public class SesameQueryTest extends QueryRunner {

    private static final Logger LOG = Logger.getLogger(SesameQueryTest.class.getName());

    private static EntityManager em;

    public SesameQueryTest() {
        super(LOG);
    }

    @BeforeClass
    public static void setUpBeforeClass() throws Exception {
        final SesamePersistenceFactory persistenceFactory = new SesamePersistenceFactory();
        em = persistenceFactory.getEntityManager("SPARQLQueryTests", false,
                Collections.singletonMap(SesameOntoDriverProperties.SESAME_USE_INFERENCE, "true"));
        QueryTestEnvironment.generateTestData(em);
        em.clear();
        em.getEntityManagerFactory().getCache().evictAll();
    }

    @AfterClass
    public static void tearDownAfterClass() throws Exception {
        em.close();
        em.getEntityManagerFactory().close();
    }

    @Override
    protected EntityManager getEntityManager() {
        return em;
    }
}

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
package cz.cvut.kbss.jopa.test.query.jena;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.test.environment.JenaPersistenceFactory;
import cz.cvut.kbss.jopa.test.query.QueryTestEnvironment;
import cz.cvut.kbss.jopa.test.query.runner.QueryRunner;
import cz.cvut.kbss.ontodriver.config.OntoDriverProperties;
import cz.cvut.kbss.ontodriver.jena.config.JenaOntoDriverProperties;
import org.apache.jena.reasoner.transitiveReasoner.TransitiveReasonerFactory;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.HashMap;
import java.util.Map;

public class QueryTest extends QueryRunner {

    private static final Logger LOG = LoggerFactory.getLogger(QueryTest.class);

    private static EntityManager em;

    QueryTest() {
        super(LOG);
    }

    @BeforeEach
    void setUp() {
        final JenaPersistenceFactory persistenceFactory = new JenaPersistenceFactory();
        final Map<String, String> properties = new HashMap<>();
        properties.put(JenaOntoDriverProperties.JENA_STORAGE_TYPE, JenaOntoDriverProperties.IN_MEMORY);
        properties.put(JenaOntoDriverProperties.JENA_TREAT_DEFAULT_GRAPH_AS_UNION, Boolean.toString(true));
        properties.put(OntoDriverProperties.REASONER_FACTORY_CLASS, TransitiveReasonerFactory.class.getCanonicalName());
        em = persistenceFactory.getEntityManager("SPARQLQueryTests", false, properties);
        QueryTestEnvironment.generateTestData(em);
        em.clear();
        em.getEntityManagerFactory().getCache().evictAll();
    }

    @AfterEach
    void tearDown() {
        em.close();
        em.getEntityManagerFactory().close();
    }

    @Override
    protected EntityManager getEntityManager() {
        return em;
    }
}

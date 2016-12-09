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
package cz.cvut.kbss.jopa.test.query;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.Vocabulary;
import cz.cvut.kbss.jopa.test.environment.Generators;
import cz.cvut.kbss.jopa.test.environment.SesamePersistenceFactory;
import cz.cvut.kbss.jopa.test.query.runner.QueryRunner;
import cz.cvut.kbss.ontodriver.sesame.config.SesameOntoDriverProperties;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.net.URI;
import java.util.Collections;

import static org.junit.Assert.*;

public class SesameQueryTest extends QueryRunner {

    private static final Logger LOG = LoggerFactory.getLogger(SesameQueryTest.class);

    private static EntityManager em;

    public SesameQueryTest() {
        super(LOG);
    }

    @Before
    public void setUp() throws Exception {
        final SesamePersistenceFactory persistenceFactory = new SesamePersistenceFactory();
        em = persistenceFactory.getEntityManager("SPARQLQueryTests", false,
                Collections.singletonMap(SesameOntoDriverProperties.SESAME_USE_INFERENCE, "true"));
        QueryTestEnvironment.generateTestData(em);
        em.clear();
        em.getEntityManagerFactory().getCache().evictAll();
    }

    @After
    public void tearDown() throws Exception {
        em.close();
        em.getEntityManagerFactory().close();
    }

    @Override
    protected EntityManager getEntityManager() {
        return em;
    }

    @Test
    public void executeUpdateRunsUpdateOnRepository() {
        final OWLClassA instance = QueryTestEnvironment.getData(OWLClassA.class).get(0);
        final String newValue = "UpdatedValue";
        final String update = "DELETE { ?inst ?property ?origValue . }" +
                "INSERT { ?inst ?property ?newValue . } WHERE {" +
                "?inst ?property ?origValue . }";
        em.createNativeQuery(update).setParameter("inst", instance.getUri()).setParameter("property", URI.create(
                Vocabulary.pAStringAttribute)).setParameter("newValue", newValue, "en").executeUpdate();

        final OWLClassA result = em.find(OWLClassA.class, instance.getUri());
        assertEquals(newValue, result.getStringAttribute());
    }

    @Test
    public void executeUpdateRunsDeleteOnRepository() {
        final OWLClassA instance = QueryTestEnvironment.getData(OWLClassA.class).get(0);
        assertNotNull(instance.getStringAttribute());
        final String update = "DELETE { ?inst ?property ?origValue . } WHERE {" +
                "?inst ?property ?origValue . }";
        em.createNativeQuery(update).setParameter("inst", instance.getUri())
          .setParameter("property", URI.create(Vocabulary.pAStringAttribute)).executeUpdate();

        final OWLClassA result = em.find(OWLClassA.class, instance.getUri());
        assertNull(result.getStringAttribute());
    }

    @Test
    public void executeUpdateRunsInsertOnRepository() {
        final URI newType = Generators.generateUri();
        final OWLClassA instance = QueryTestEnvironment.getData(OWLClassA.class).get(0);
        final String update = "INSERT DATA { ?inst a ?newType . }";
        em.createNativeQuery(update).setParameter("inst", instance.getUri())
          .setParameter("newType", newType).executeUpdate();

        final OWLClassA result = em.find(OWLClassA.class, instance.getUri());
        assertTrue(result.getTypes().contains(newType.toString()));
    }
}

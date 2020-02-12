/**
 * Copyright (C) 2020 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.test.runner;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.test.*;
import cz.cvut.kbss.jopa.test.environment.DataAccessor;
import cz.cvut.kbss.jopa.test.environment.PersistenceFactory;
import cz.cvut.kbss.jopa.test.environment.Quad;
import org.junit.jupiter.api.AfterEach;
import org.slf4j.Logger;

import java.net.URI;
import java.util.*;

import static org.junit.jupiter.api.Assertions.*;


public abstract class BaseRunner {

    protected static final URI CONTEXT_ONE = URI.create("http://krizik.felk.cvut.cz/jopa/contexts#One");
    protected static final URI CONTEXT_TWO = URI.create("http://krizik.felk.cvut.cz/jopa/contexts#Two");


    protected final Logger logger;

    protected EntityManager em;

    protected OWLClassA entityA;
    protected OWLClassB entityB;
    protected OWLClassC entityC;
    protected OWLClassD entityD;
    // Generated IRI
    protected OWLClassE entityE;
    // Two relationships, cascade
    protected OWLClassG entityG;
    protected OWLClassH entityH;
    // Lazy reference to OWLClassA
    protected OWLClassI entityI;
    protected OWLClassM entityM;
    protected OWLClassN entityN;
    protected OWLClassP entityP;
    // Mapped superclass
    protected OWLClassQ entityQ;

    protected final DataAccessor dataAccessor;
    protected final PersistenceFactory persistenceFactory;

    public BaseRunner(Logger logger, PersistenceFactory persistenceFactory, DataAccessor dataAccessor) {
        assert logger != null;
        this.logger = logger;
        this.persistenceFactory = persistenceFactory;
        this.dataAccessor = dataAccessor;
        init();
    }

    /**
     * Initializes the test entities in the following manner:
     * <pre>
     * <ul>
     *     <li>entityA contains non-empty types</li>
     *     <li>entityB's properties are null</li>
     *     <li>entityC's simple and referenced lists are null</li>
     *     <li>entityD's reference to OWLClassA is set to entityA</li>
     *     <li>entityE's URI is left null for ID generation</li>
     *     <li>entityG's reference to OWLClassH is set to entityH</li>
     *     <li>entityH's reference to OWLClassA is set to entityA</li>
     *     <li>entityI's reference to OWLClassA is set to entityA</li>
     *     <li>entityM has all fields set to some values</li>
     *     <li>entityN has required fields set</li>
     *     <li>entityP's properties and uri are null</li>
     *     <li>entityQ's reference to OWLClassA is set to entityA</li>
     * </ul>
     * </pre>
     */
    private void init() {
        this.entityA = new OWLClassA(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityA"));
        entityA.setStringAttribute("entityAStringAttribute");
        final Set<String> types = new HashSet<>();
        types.add("http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassU");
        entityA.setTypes(types);
        this.entityB = new OWLClassB();
        entityB.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityB"));
        entityB.setStringAttribute("entityBStringAttribute");
        this.entityC = new OWLClassC();
        entityC.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityC"));
        this.entityD = new OWLClassD(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityD"));
        entityD.setOwlClassA(entityA);
        this.entityE = new OWLClassE();
        entityE.setStringAttribute("entityEStringAttribute");
        this.entityH = new OWLClassH();
        entityH.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityH"));
        entityH.setOwlClassA(entityA);
        this.entityG = new OWLClassG();
        entityG.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityG"));
        entityG.setOwlClassH(entityH);
        this.entityI = new OWLClassI();
        entityI.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityI"));
        entityI.setOwlClassA(entityA);
        this.entityM = new OWLClassM();
        entityM.initializeTestValues(true);
        this.entityN = new OWLClassN();
        entityN.setStringAttribute("entityNStringAttribute");
        this.entityP = new OWLClassP();
        this.entityQ = new OWLClassQ();
        entityQ.setStringAttribute("entityQStringAttribute");
        entityQ.setParentString("entityQParentStringAttribute");
        entityQ.setLabel("entityQLabel");
        entityQ.setOwlClassA(entityA);
    }

    @AfterEach
    public void tearDown() {
        if (em != null && em.isOpen()) {
            if (em.getTransaction().isActive()) {
                em.getTransaction().rollback();
            }
            em.close();
            em.getEntityManagerFactory().close();
        }
    }

    /**
     * Persists the specified instance(s) in a separate transaction.
     *
     * @param entity Entity to persist
     */
    protected void persist(Object... entity) {
        em.getTransaction().begin();
        for (Object ent : entity) {
            em.persist(ent);
        }
        em.getTransaction().commit();
    }

    /**
     * Runs the specified action in a transaction on the current entity manager.
     *
     * @param action The code to run
     */
    protected void transactional(Runnable action) {
        em.getTransaction().begin();
        action.run();
        em.getTransaction().commit();
    }

    /**
     * Verifies that no statements with the specified individual as subject exist in the ontology any more.
     *
     * @param identifier Individual identifier
     */
    protected void verifyIndividualWasRemoved(URI identifier) {
        // TODO There is a bug in OWL2Query - the query returns true, because it finds the top object and data property assertion for an individual
        // which doesn't exist anymore (but is a part of the query)
        final boolean remains = em.createNativeQuery("ASK WHERE { ?instance ?y ?z . }", Boolean.class)
                                  .setParameter("instance", identifier).getSingleResult();
        assertFalse(remains);
    }

    protected EntityManager getEntityManager(String repositoryName, boolean cacheEnabled) {
        return getEntityManager(repositoryName, cacheEnabled, Collections.emptyMap());
    }

    protected EntityManager getEntityManager(String repositoryName, boolean cacheEnabled,
                                             Map<String, String> properties) {
        return persistenceFactory.getEntityManager(repositoryName, cacheEnabled, properties);
    }

    protected void persistTestData(Collection<Quad> data, EntityManager em) throws Exception {
        dataAccessor.persistTestData(data, em);
    }

    protected void verifyStatementsPresent(Collection<Quad> expected, EntityManager em) throws Exception {
        dataAccessor.verifyDataPresent(expected, em);
    }

    protected void verifyStatementsNotPresent(Collection<Quad> notExpected, EntityManager em) throws Exception {
        dataAccessor.verifyDataNotPresent(notExpected, em);
    }

    // Utility methods to reduce duplication

    /**
     * Persists the specified instance of {@link OWLClassC} together will all items in the lists (if specified).
     *
     * @param instance Instace to persist
     */
    void persistCWithLists(OWLClassC instance) {
        transactional(() -> {
            em.persist(instance);
            if (instance.getSimpleList() != null) {
                instance.getSimpleList().forEach(em::persist);
            }
            if (instance.getReferencedList() != null) {
                instance.getReferencedList().forEach(em::persist);
            }
        });
    }

    /**
     * Finds instance and checks for it not being {@code null}.
     *
     * @return The found instance
     */
    <T> T findRequired(Class<T> type, Object identifier) {
        final T result = em.find(type, identifier);
        assertNotNull(result);
        return result;
    }

    /**
     * Finds instance and checks for it not being {@code null}.
     *
     * @return The found instance
     */
    <T> T findRequired(Class<T> type, Object identifier, Descriptor descriptor) {
        final T result = em.find(type, identifier, descriptor);
        assertNotNull(result);
        return result;
    }

    <T> void verifyExists(Class<T> type, Object identifier) {
        assertNotNull(em.find(type, identifier));
    }

    protected void verifyValueDatatype(URI identifier, String property, String expectedDatatype) {
        assertTrue(em.createNativeQuery("ASK WHERE { ?x ?property ?value . " +
                "FILTER (DATATYPE(?value) = ?datatype) }", Boolean.class)
                     .setParameter("x", identifier)
                     .setParameter("property", URI.create(property))
                     .setParameter("datatype", URI.create(expectedDatatype)).getSingleResult());
    }
}

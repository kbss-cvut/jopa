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
package cz.cvut.kbss.jopa.test.runner;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.test.*;
import org.junit.After;

import java.net.URI;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.logging.Logger;

public abstract class BaseRunner {

    protected static final URI CONTEXT_ONE = URI
            .create("http://krizik.felk.cvut.cz/jopa/contexts#One");
    protected static final URI CONTEXT_TWO = URI
            .create("http://krizik.felk.cvut.cz/jopa/contexts#Two");


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

    public BaseRunner(Logger logger) {
        assert logger != null;
        this.logger = logger;
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
     * </ul>
     * </pre>
     */
    private void init() {
        this.entityA = new OWLClassA();
        entityA.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityA"));
        entityA.setStringAttribute("entityAStringAttribute");
        final Set<String> types = new HashSet<>();
        types.add("http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassU");
        entityA.setTypes(types);
        this.entityB = new OWLClassB();
        entityB.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityB"));
        entityB.setStringAttribute("entityBStringAttribute");
        this.entityC = new OWLClassC();
        entityC.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityC"));
        this.entityD = new OWLClassD();
        entityD.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityD"));
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
    }

    @After
    public void tearDown() throws Exception {
        assert em != null;
        if (em.isOpen()) {
            if (em.getTransaction().isActive()) {
                em.getTransaction().rollback();
            }
            em.close();
            em.getEntityManagerFactory().close();
        }
    }

    /**
     * Persists the specified instance in a separate transaction.
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

    protected abstract EntityManager getEntityManager(String repositoryName, boolean cacheEnabled);

    protected abstract EntityManager getEntityManager(String repositoryName, boolean cacheEnabled,
                                                      Map<String, String> properties);
}

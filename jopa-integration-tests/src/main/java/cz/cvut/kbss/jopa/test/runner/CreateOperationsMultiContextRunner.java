/**
 * Copyright (C) 2019 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.test.runner;

import cz.cvut.kbss.jopa.exceptions.OWLEntityExistsException;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.descriptors.ObjectPropertyCollectionDescriptor;
import cz.cvut.kbss.jopa.model.query.TypedQuery;
import cz.cvut.kbss.jopa.test.*;
import cz.cvut.kbss.jopa.test.environment.DataAccessor;
import cz.cvut.kbss.jopa.test.environment.Generators;
import cz.cvut.kbss.jopa.test.environment.PersistenceFactory;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;

import java.net.URI;
import java.util.Map;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.*;

public abstract class CreateOperationsMultiContextRunner extends BaseRunner {

    private OWLClassF entityF;
    private OWLClassK entityK;

    private EntityDescriptor cOneDescriptor = new EntityDescriptor(CONTEXT_ONE);
    private EntityDescriptor cTwoDescriptor = new EntityDescriptor(CONTEXT_TWO);

    public CreateOperationsMultiContextRunner(Logger logger, PersistenceFactory persistenceFactory,
                                              DataAccessor dataAccessor) {
        super(logger, persistenceFactory, dataAccessor);
        initialize();
    }

    private void initialize() {
        this.entityF = new OWLClassF();
        entityF.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityF"));
        this.entityK = new OWLClassK();
        entityK.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityK"));
    }

    @Test
    void testPersistDataPropertyIntoContext() throws Exception {
        this.em = getEntityManager("MultiPersistDataPropertyIntoContext", false);
        final Descriptor aDescriptor = new EntityDescriptor();
        aDescriptor.addAttributeContext(OWLClassA.class.getDeclaredField("stringAttribute"), CONTEXT_ONE);
        em.getTransaction().begin();
        em.persist(entityA, aDescriptor);
        em.getTransaction().commit();

        final OWLClassA res = findRequired(OWLClassA.class, entityA.getUri(), aDescriptor);
        assertEquals(entityA.getUri(), res.getUri());
        assertEquals(entityA.getStringAttribute(), res.getStringAttribute());
        assertEquals(entityA.getTypes().size(), res.getTypes().size());
        assertTrue(entityA.getTypes().containsAll(res.getTypes()));
    }

    @Test
    void testPersistObjectPropertyIntoContext() throws Exception {
        this.em = getEntityManager("MultiPersistObjectPropertyIntoContext", false);
        final Descriptor dDescriptor = new EntityDescriptor(false);
        dDescriptor.addAttributeDescriptor(OWLClassD.getOwlClassAField(), cOneDescriptor);
        em.getTransaction().begin();
        em.persist(entityD, dDescriptor);
        em.persist(entityA, cOneDescriptor);
        em.getTransaction().commit();

        final OWLClassD resD = findRequired(OWLClassD.class, entityD.getUri(), dDescriptor);
        assertNotNull(resD.getOwlClassA());
        final OWLClassA resA = findRequired(OWLClassA.class, entityA.getUri(), cOneDescriptor);
        assertEquals(resD.getOwlClassA().getUri(), resA.getUri());
        assertEquals(resD.getOwlClassA().getStringAttribute(), resA.getStringAttribute());
        assertTrue(resD.getOwlClassA().getTypes().containsAll(resA.getTypes()));
    }

    @Test
    void persistWithObjectPropertySavesAssertionIntoSubjectContextWhenConfiguredTo() throws Exception {
        this.em = getEntityManager("persistWithObjectPropertySavesAssertionIntoSubjectContextWhenConfiguredTo", false);
        cOneDescriptor.addAttributeDescriptor(OWLClassD.getOwlClassAField(), cTwoDescriptor);
        em.getTransaction().begin();
        em.persist(entityD, cOneDescriptor);
        em.persist(entityA, cTwoDescriptor);
        em.getTransaction().commit();

        final TypedQuery<Boolean> query = em.createNativeQuery("ASK {GRAPH ?g {?d ?hasA ?a .}}", Boolean.class)
                                            .setParameter("g", CONTEXT_ONE).setParameter("d", entityD.getUri())
                                            .setParameter("hasA", URI.create(Vocabulary.P_HAS_OWL_CLASS_A))
                                            .setParameter("a", entityA.getUri());
        assertTrue(query.getSingleResult());
        query.setParameter("g", CONTEXT_TWO);
        assertFalse(query.getSingleResult());
    }

    @Test
    void testPersistWithGeneratedIntoContext() {
        this.em = getEntityManager("MultiPersistWithGeneratedIntoContext", false);
        em.getTransaction().begin();
        em.persist(entityE, cOneDescriptor);
        em.getTransaction().commit();

        final OWLClassE res = findRequired(OWLClassE.class, entityE.getUri(), cOneDescriptor);
        assertEquals(entityE.getUri(), res.getUri());
        assertEquals(entityE.getStringAttribute(), res.getStringAttribute());
    }

    @Test
    void testPersistReferenceIntoContextAndThenOwnerIntoDefault() {
        this.em = getEntityManager("ReferenceIntoContextThenOwnerIntoDefault", false);
        em.getTransaction().begin();
        em.persist(entityA, cOneDescriptor);
        em.getTransaction().commit();

        em.clear();
        em.getTransaction().begin();
        entityD.setOwlClassA(entityA);
        em.persist(entityD);
        em.getTransaction().commit();

        final OWLClassD res = findRequired(OWLClassD.class, entityD.getUri());
        assertNotNull(res.getOwlClassA());
    }

    @Test
    void persistTwiceIntoSameContextIsInvalid() {
        this.em = getEntityManager("MultiPersistTwiceIntoOneContext", false);
        em.getTransaction().begin();
        em.persist(entityA, cOneDescriptor);
        em.getTransaction().commit();

        assertThrows(OWLEntityExistsException.class, () -> {
            em.getTransaction().begin();
            em.persist(entityA, cOneDescriptor);
            em.getTransaction().commit();
        });
    }

    @Test
    void persistTwiceIntoDifferentContextsIsLegal() {
        this.em = getEntityManager("MultiPersistTwiceIntoDifferentContexts", false);
        em.getTransaction().begin();
        em.persist(entityA, cOneDescriptor);
        em.getTransaction().commit();
        em.getTransaction().begin();
        em.persist(entityA, cTwoDescriptor);
        em.getTransaction().commit();

        final OWLClassA resOne = findRequired(OWLClassA.class, entityA.getUri(), cOneDescriptor);
        final OWLClassA resTwo = findRequired(OWLClassA.class, entityA.getUri(), cTwoDescriptor);
        assertNotSame(resOne, resTwo);
        assertEquals(resOne.getUri(), resTwo.getUri());
        assertEquals(resOne.getStringAttribute(), resTwo.getStringAttribute());
    }

    @Test
    void testPersistPropertiesIntoDifferentContext() throws Exception {
        this.em = getEntityManager("MultiPersistPropertiesIntoDifferentContext", false);
        final Descriptor bDescriptor = new EntityDescriptor();
        entityB.setProperties(Generators.createProperties(10));
        bDescriptor.addAttributeContext(OWLClassB.class.getDeclaredField("properties"), CONTEXT_ONE);
        em.getTransaction().begin();
        em.persist(entityB, bDescriptor);
        em.getTransaction().commit();

        final OWLClassB res = findRequired(OWLClassB.class, entityB.getUri(), bDescriptor);
        assertEquals(entityB.getStringAttribute(), res.getStringAttribute());
        assertEquals(entityB.getProperties().size(), res.getProperties().size());
        for (Map.Entry<String, Set<String>> e : res.getProperties().entrySet()) {
            assertTrue(entityB.getProperties().containsKey(e.getKey()));
            assertEquals(e.getValue(), entityB.getProperties().get(e.getKey()));
        }
    }

    @Test
    void testPersistCascadeIntoThreeContexts() throws Exception {
        this.em = getEntityManager("MultiPersistCascadeIntoThreeContexts", false);
        final Descriptor gDescriptor = new EntityDescriptor(false);
        final Descriptor hDescriptor = new EntityDescriptor(CONTEXT_ONE, false);
        hDescriptor.addAttributeDescriptor(OWLClassH.class.getDeclaredField("owlClassA"), cTwoDescriptor);
        gDescriptor.addAttributeDescriptor(OWLClassG.class.getDeclaredField("owlClassH"), hDescriptor);
        em.getTransaction().begin();
        em.persist(entityG, gDescriptor);
        assertTrue(em.contains(entityG));
        assertTrue(em.contains(entityH));
        assertTrue(em.contains(entityA));
        em.getTransaction().commit();

        final OWLClassA resA = findRequired(OWLClassA.class, entityA.getUri(), cTwoDescriptor);
        final OWLClassH resH = findRequired(OWLClassH.class, entityH.getUri(), hDescriptor);
        assertSame(resA, resH.getOwlClassA());
        final OWLClassG resG = findRequired(OWLClassG.class, entityG.getUri(), gDescriptor);
        assertSame(resH, resG.getOwlClassH());
    }

    @Test
    void persistCascadeIntoThreeContextSavesOPAssertionAlwaysIntoSubjectContextWhenConfigured()
            throws Exception {
        this.em = getEntityManager(
                "persistCascadeIntoThreeContextSavesOPAssertionAlwaysIntoSubjectContextWhenConfigured", false);
        final Descriptor gDescriptor = new EntityDescriptor();
        cOneDescriptor.addAttributeDescriptor(OWLClassH.class.getDeclaredField("owlClassA"), cTwoDescriptor);
        gDescriptor.addAttributeDescriptor(OWLClassG.class.getDeclaredField("owlClassH"), cOneDescriptor);
        em.getTransaction().begin();
        em.persist(entityG, gDescriptor);
        assertTrue(em.contains(entityG));
        assertTrue(em.contains(entityH));
        assertTrue(em.contains(entityA));
        em.getTransaction().commit();

        assertTrue(em.createNativeQuery("ASK { ?s ?p ?o . }", Boolean.class)
                     .setParameter("s", entityG.getUri())
                     .setParameter("p", URI.create(Vocabulary.P_HAS_H))
                     .setParameter("o", entityH.getUri()).getSingleResult());
        final TypedQuery<Boolean> query = em.createNativeQuery("ASK {GRAPH ?g { ?s ?p ?o . }}", Boolean.class)
                                            .setParameter("g", CONTEXT_ONE)
                                            .setParameter("s", entityG.getUri())
                                            .setParameter("p", URI.create(Vocabulary.P_HAS_H))
                                            .setParameter("o", entityH.getUri());
        assertFalse(query.getSingleResult());
        query.setParameter("s", entityH.getUri())
             .setParameter("p", URI.create(Vocabulary.P_HAS_OWL_CLASS_A))
             .setParameter("o", entityA.getUri());
        assertTrue(query.getSingleResult());
        query.setParameter("g", CONTEXT_TWO);
        assertFalse(query.getSingleResult());
    }

    @Test
    void testPersistSetWithAttributeContexts() throws Exception {
        this.em = getEntityManager("MultiPersistSetWithAttributeContexts", false);
        entityF.setSimpleSet(Generators.createSimpleSet(20));
        final Descriptor fDescriptor = new EntityDescriptor(false);
        final ObjectPropertyCollectionDescriptor setDescriptor = new ObjectPropertyCollectionDescriptor(CONTEXT_ONE,
                OWLClassF.class.getDeclaredField("simpleSet"), false);
        fDescriptor.addAttributeDescriptor(OWLClassF.class.getDeclaredField("simpleSet"), setDescriptor);
        setDescriptor.addAttributeContext(OWLClassA.class.getDeclaredField("stringAttribute"), CONTEXT_TWO);
        setDescriptor.addAttributeContext(OWLClassA.class.getDeclaredField("types"), CONTEXT_TWO);
        em.getTransaction().begin();
        em.persist(entityF, fDescriptor);
        for (OWLClassA a : entityF.getSimpleSet()) {
            em.persist(a, setDescriptor.getElementDescriptor());
        }
        em.getTransaction().commit();

        final OWLClassF resF = findRequired(OWLClassF.class, entityF.getUri(), fDescriptor);
        assertEquals(entityF.getSimpleSet().size(), resF.getSimpleSet().size());
        for (OWLClassA a : resF.getSimpleSet()) {
            final OWLClassA resA = findRequired(OWLClassA.class, a.getUri(), setDescriptor);
            assertEquals(a.getStringAttribute(), resA.getStringAttribute());
            assertEquals(a.getTypes(), resA.getTypes());
        }
    }

    @Test
    void testPersistEntityWithObjectPropertyWithGeneratedIdentifierAndPutTheReferenceIntoContext()
            throws Exception {
        this.em = getEntityManager("PersistEntityWithObjectPropertyWithGeneratedIdentifierContexts", true);
        entityK.setOwlClassE(entityE);
        assertNull(entityE.getUri());
        cOneDescriptor.addAttributeDescriptor(OWLClassK.class.getDeclaredField("owlClassE"), cTwoDescriptor);
        em.getTransaction().begin();
        em.persist(entityK, cOneDescriptor);
        em.persist(entityE, cTwoDescriptor);
        assertNotNull(entityE.getUri());
        em.getTransaction().commit();

        final OWLClassE resE = findRequired(OWLClassE.class, entityE.getUri(), cTwoDescriptor);
        assertEquals(entityE.getStringAttribute(), resE.getStringAttribute());
        final OWLClassK resK = findRequired(OWLClassK.class, entityK.getUri(), cOneDescriptor);
        assertEquals(resE, resK.getOwlClassE());
    }

    @Test
    void testPersistEntityWithMappedSuperclassPuttingReferenceIntoDifferentContext() throws Exception {
        this.em = getEntityManager("PersistEntityWithMappedSuperclassReferenceInContext", true);
        cOneDescriptor.addAttributeDescriptor(OWLClassQ.getOWlClassAField(), cTwoDescriptor);
        cOneDescriptor.addAttributeDescriptor(OWLClassQ.getOWlClassAField(), cTwoDescriptor);
        em.getTransaction().begin();
        em.persist(entityQ, cOneDescriptor);
        em.persist(entityA, cTwoDescriptor);
        em.getTransaction().commit();

        final OWLClassA resA = findRequired(OWLClassA.class, entityA.getUri(), cTwoDescriptor);
        final OWLClassQ resQ = findRequired(OWLClassQ.class, entityQ.getUri(), cOneDescriptor);
        assertEquals(entityQ.getStringAttribute(), resQ.getStringAttribute());
        assertEquals(entityQ.getParentString(), resQ.getParentString());
        assertEquals(entityQ.getLabel(), resQ.getLabel());
        assertNotNull(resQ.getOwlClassA());
        assertEquals(resA, resQ.getOwlClassA());
    }

    @Test
    void persistReferenceIntoContextBeforeOwnerDoesNotThrowUnpersistedChangeException() throws Exception {
        this.em =
                getEntityManager("persistReferenceIntoContextBeforeOwnerDoesNotThrowUnpersistedChangeException", false);
        cOneDescriptor.addAttributeDescriptor(OWLClassD.getOwlClassAField(), new EntityDescriptor(null));
        persist(entityA);
        transactional(() -> em.persist(entityD, cOneDescriptor));

        final OWLClassD result = findRequired(OWLClassD.class, entityD.getUri(), cOneDescriptor);
        assertNotNull(result.getOwlClassA());
        assertEquals(entityA.getStringAttribute(), result.getOwlClassA().getStringAttribute());
    }
}

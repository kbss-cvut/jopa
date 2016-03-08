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
package cz.cvut.kbss.jopa.test.runner;

import cz.cvut.kbss.jopa.exceptions.OWLEntityExistsException;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.descriptors.ObjectPropertyCollectionDescriptor;
import cz.cvut.kbss.jopa.test.*;
import cz.cvut.kbss.jopa.test.environment.Generators;
import org.junit.Test;
import org.slf4j.Logger;

import java.net.URI;
import java.util.Map;
import java.util.Set;

import static org.junit.Assert.*;

public abstract class CreateOperationsMultiContextRunner extends BaseRunner {

    private OWLClassF entityF;
    private OWLClassK entityK;

    public CreateOperationsMultiContextRunner(Logger logger) {
        super(logger);
        initialize();
    }

    private void initialize() {
        this.entityF = new OWLClassF();
        entityF.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityF"));
        this.entityK = new OWLClassK();
        entityK.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityK"));
    }

    @Test
    public void testPersistDataPropertyIntoContext() throws Exception {
        logger.debug("Test: persist an entity into the default context and its data property into a different one.");
        this.em = getEntityManager("MultiPersistDataPropertyIntoContext", false);
        final Descriptor aDescriptor = new EntityDescriptor();
        aDescriptor.addAttributeContext(OWLClassA.class.getDeclaredField("stringAttribute"), CONTEXT_ONE);
        em.getTransaction().begin();
        em.persist(entityA, aDescriptor);
        em.getTransaction().commit();

        final OWLClassA res = em.find(OWLClassA.class, entityA.getUri(), aDescriptor);
        assertNotNull(res);
        assertEquals(entityA.getUri(), res.getUri());
        assertEquals(entityA.getStringAttribute(), res.getStringAttribute());
        assertEquals(entityA.getTypes().size(), res.getTypes().size());
        assertTrue(entityA.getTypes().containsAll(res.getTypes()));
    }

    @Test
    public void testPersistObjectPropertyIntoContext() throws Exception {
        logger.debug(
                "Test: persist entity into one context and its object property into another, along with its own attributes.");
        this.em = getEntityManager("MultiPersistObjectPropertyIntoContext", false);
        final Descriptor dDescriptor = new EntityDescriptor();
        final Descriptor aDescriptor = new EntityDescriptor(CONTEXT_ONE);
        dDescriptor.addAttributeDescriptor(OWLClassD.class.getDeclaredField("owlClassA"), aDescriptor);
        em.getTransaction().begin();
        em.persist(entityD, dDescriptor);
        em.persist(entityA, aDescriptor);
        em.getTransaction().commit();

        final OWLClassD resD = em.find(OWLClassD.class, entityD.getUri(), dDescriptor);
        assertNotNull(resD);
        assertNotNull(resD.getOwlClassA());
        final OWLClassA resA = em.find(OWLClassA.class, entityA.getUri(), aDescriptor);
        assertNotNull(resA);
        assertEquals(resD.getOwlClassA().getUri(), resA.getUri());
        assertEquals(resD.getOwlClassA().getStringAttribute(), resA.getStringAttribute());
        assertTrue(resD.getOwlClassA().getTypes().containsAll(resA.getTypes()));
    }

    @Test
    public void testPersistWithGeneratedIntoContext() {
        logger.debug("Test: persist entity with generated ID into a context.");
        this.em = getEntityManager("MultiPersistWithGeneratedIntoContext", false);
        final Descriptor eDescriptor = new EntityDescriptor(CONTEXT_ONE);
        em.getTransaction().begin();
        em.persist(entityE, eDescriptor);
        em.getTransaction().commit();

        final OWLClassE res = em.find(OWLClassE.class, entityE.getUri(), eDescriptor);
        assertNotNull(res);
        assertEquals(entityE.getUri(), res.getUri());
        assertEquals(entityE.getStringAttribute(), res.getStringAttribute());
    }

    @Test(expected = OWLEntityExistsException.class)
    public void testPersistTwiceIntoOneContext() {
        this.em = getEntityManager("MultiPersistTwiceIntoOneContext", false);
        logger.debug("Test: persist an entity twice into the same context.");
        final Descriptor aDescriptor = new EntityDescriptor(CONTEXT_ONE);
        em.getTransaction().begin();
        em.persist(entityA, aDescriptor);
        em.getTransaction().commit();

        assertNotNull(em.find(OWLClassA.class, entityA.getUri(), aDescriptor));
        em.getTransaction().begin();
        em.persist(entityA, aDescriptor);
        em.getTransaction().commit();
    }

    @Test
    public void testPersistTwiceIntoDifferentContexts() {
        this.em = getEntityManager("MultiPersistTwiceIntoDifferentContexts", false);
        logger.debug("Test: persist an entity into two different contexts.");
        final Descriptor aDescriptorOne = new EntityDescriptor(CONTEXT_ONE);
        final Descriptor aDescriptorTwo = new EntityDescriptor(CONTEXT_TWO);
        em.getTransaction().begin();
        em.persist(entityA, aDescriptorOne);
        em.persist(entityA, aDescriptorTwo);
        em.getTransaction().commit();

        final OWLClassA resOne = em.find(OWLClassA.class, entityA.getUri(), aDescriptorOne);
        assertNotNull(resOne);
        final OWLClassA resTwo = em.find(OWLClassA.class, entityA.getUri(), aDescriptorTwo);
        assertNotNull(resTwo);
        assertNotSame(resOne, resTwo);
        assertEquals(resOne.getUri(), resTwo.getUri());
        assertEquals(resOne.getStringAttribute(), resTwo.getStringAttribute());
    }

    @Test
    public void testPersistPropertiesIntoDifferentContext() throws Exception {
        this.em = getEntityManager("MultiPersistPropertiesIntoDifferentContext", false);
        logger.debug("Test: persist an entity and persist its properties into a different context.");
        final Descriptor bDescriptor = new EntityDescriptor();
        entityB.setProperties(Generators.createProperties(10));
        bDescriptor.addAttributeContext(OWLClassB.class.getDeclaredField("properties"), CONTEXT_ONE);
        em.getTransaction().begin();
        em.persist(entityB, bDescriptor);
        em.getTransaction().commit();

        final OWLClassB res = em.find(OWLClassB.class, entityB.getUri(), bDescriptor);
        assertNotNull(res);
        assertEquals(entityB.getStringAttribute(), res.getStringAttribute());
        assertEquals(entityB.getProperties().size(), res.getProperties().size());
        for (Map.Entry<String, Set<String>> e : res.getProperties().entrySet()) {
            assertTrue(entityB.getProperties().containsKey(e.getKey()));
            assertEquals(e.getValue(), entityB.getProperties().get(e.getKey()));
        }
    }

    @Test
    public void testPersistCascadeIntoThreeContexts() throws Exception {
        this.em = getEntityManager("MultiPersistCascadeIntoThreeContexts", false);
        logger.debug("Test: persist three entities in cascaded relationship, each into a different context.");
        final Descriptor gDescriptor = new EntityDescriptor();
        final Descriptor hDescriptor = new EntityDescriptor(CONTEXT_ONE);
        final Descriptor aDescriptor = new EntityDescriptor(CONTEXT_TWO);
        hDescriptor.addAttributeDescriptor(OWLClassH.class.getDeclaredField("owlClassA"), aDescriptor);
        gDescriptor.addAttributeDescriptor(OWLClassG.class.getDeclaredField("owlClassH"), hDescriptor);
        em.getTransaction().begin();
        em.persist(entityG, gDescriptor);
        assertTrue(em.contains(entityG));
        assertTrue(em.contains(entityH));
        assertTrue(em.contains(entityA));
        em.getTransaction().commit();

        final OWLClassA resA = em.find(OWLClassA.class, entityA.getUri(), aDescriptor);
        assertNotNull(resA);
        final OWLClassH resH = em.find(OWLClassH.class, entityH.getUri(), hDescriptor);
        assertNotNull(resH);
        assertSame(resA, resH.getOwlClassA());
        final OWLClassG resG = em.find(OWLClassG.class, entityG.getUri(), gDescriptor);
        assertNotNull(resG);
        assertSame(resH, resG.getOwlClassH());
    }

    @Test
    public void testPersistSetWithAttributeContexts() throws Exception {
        logger.debug(
                "Test: persist entity with simple set, the set will be in a different context and attributes of its element in another.");
        this.em = getEntityManager("MultiPersistSetWithAttributeContexts", false);
        entityF.setSimpleSet(Generators.createSimpleSet(20));
        final Descriptor fDescriptor = new EntityDescriptor();
        final Descriptor setDescriptor = new ObjectPropertyCollectionDescriptor(CONTEXT_ONE,
                OWLClassF.class.getDeclaredField("simpleSet"));
        fDescriptor.addAttributeDescriptor(OWLClassF.class.getDeclaredField("simpleSet"), setDescriptor);
        setDescriptor.addAttributeContext(OWLClassA.class.getDeclaredField("stringAttribute"), CONTEXT_TWO);
        setDescriptor.addAttributeContext(OWLClassA.class.getDeclaredField("types"), CONTEXT_TWO);
        em.getTransaction().begin();
        em.persist(entityF, fDescriptor);
        for (OWLClassA a : entityF.getSimpleSet()) {
            em.persist(a, setDescriptor);
        }
        em.getTransaction().commit();

        final OWLClassF resF = em.find(OWLClassF.class, entityF.getUri(), fDescriptor);
        assertNotNull(resF);
        assertEquals(entityF.getSimpleSet().size(), resF.getSimpleSet().size());
        for (OWLClassA a : resF.getSimpleSet()) {
            final OWLClassA resA = em.find(OWLClassA.class, a.getUri(), setDescriptor);
            assertNotNull(resA);
            assertEquals(a.getStringAttribute(), resA.getStringAttribute());
            assertEquals(a.getTypes(), resA.getTypes());
        }
    }

    @Test
    public void testPersistEntityWithObjectPropertyWithGeneratedIdentifierAndPutTheReferenceIntoContext()
            throws Exception {
        logger.debug("Test: persist entity with reference to an entity with generated identifier. "
                + "The identifier should be generated automatically before the referenced entity itself is persisted.");
        this.em = getEntityManager("PersistEntityWithObjectPropertyWithGeneratedIdentifierContexts", true);
        entityK.setOwlClassE(entityE);
        assertNull(entityE.getUri());
        final Descriptor eDescriptor = new EntityDescriptor(CONTEXT_TWO);
        final Descriptor kDescriptor = new EntityDescriptor(CONTEXT_ONE);
        kDescriptor.addAttributeDescriptor(OWLClassK.class.getDeclaredField("owlClassE"), eDescriptor);
        em.getTransaction().begin();
        em.persist(entityK, kDescriptor);
        assertNotNull(entityE.getUri());
        em.persist(entityE, eDescriptor);
        em.getTransaction().commit();

        final OWLClassE resE = em.find(OWLClassE.class, entityE.getUri(), eDescriptor);
        assertNotNull(resE);
        assertEquals(entityE.getStringAttribute(), resE.getStringAttribute());
        final OWLClassK resK = em.find(OWLClassK.class, entityK.getUri(), kDescriptor);
        assertNotNull(resK);
        assertEquals(resE, resK.getOwlClassE());
    }
}

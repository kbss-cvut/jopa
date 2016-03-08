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

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.descriptors.ObjectPropertyCollectionDescriptor;
import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.OWLClassB;
import cz.cvut.kbss.jopa.test.OWLClassC;
import cz.cvut.kbss.jopa.test.OWLClassI;
import cz.cvut.kbss.jopa.test.environment.Generators;
import cz.cvut.kbss.jopa.test.environment.TestEnvironmentUtils;
import org.junit.Test;
import org.slf4j.Logger;

import java.lang.reflect.Field;

import static org.junit.Assert.*;

public abstract class RetrieveOperationsMultiContextRunner extends BaseRunner {

    public RetrieveOperationsMultiContextRunner(Logger logger) {
        super(logger);
    }

    @Test
    public void testRetrieveSimilarFromTwoContexts() throws Exception {
        logger.debug(
                "Test: persist entities with the same URI but different attributes into two contexts and then retrieve them.");
        this.em = getEntityManager("MultiRetrieveSimilarFromTwoContexts", false);
        final OWLClassA entityATwo = new OWLClassA();
        entityATwo.setUri(entityA.getUri());
        entityATwo.setStringAttribute("SomeCompletelyDifferentStringAttribute");
        final Descriptor aDescriptor = new EntityDescriptor(CONTEXT_ONE);
        final Descriptor aTwoDescriptor = new EntityDescriptor(CONTEXT_TWO);
        em.getTransaction().begin();
        em.persist(entityA, aDescriptor);
        em.persist(entityATwo, aTwoDescriptor);
        em.getTransaction().commit();

        final OWLClassA resOne = em.find(OWLClassA.class, entityA.getUri(), aDescriptor);
        assertNotNull(resOne);
        assertEquals(entityA.getStringAttribute(), resOne.getStringAttribute());
        final OWLClassA resTwo = em.find(OWLClassA.class, entityATwo.getUri(), aTwoDescriptor);
        assertNotNull(resTwo);
        assertEquals(entityATwo.getStringAttribute(), resTwo.getStringAttribute());
    }

    @Test
    public void testRetrieveSimpleListFromContext() throws Exception {
        logger.debug("Test: retrieve simple list and its values from a different context.");
        this.em = getEntityManager("MultiRetrieveSimpleListFromContext", false);
        entityC.setSimpleList(Generators.createSimpleList(10));
        final Descriptor cDescriptor = new EntityDescriptor();
        final Descriptor listDescriptor = new ObjectPropertyCollectionDescriptor(CONTEXT_ONE,
                OWLClassC.class.getDeclaredField("simpleList"));
        cDescriptor.addAttributeDescriptor(OWLClassC.class.getDeclaredField("simpleList"), listDescriptor);
        em.getTransaction().begin();
        em.persist(entityC, cDescriptor);
        for (OWLClassA a : entityC.getSimpleList()) {
            em.persist(a, listDescriptor);
        }
        em.getTransaction().commit();

        final OWLClassC resC = em.find(OWLClassC.class, entityC.getUri(), cDescriptor);
        assertNotNull(resC);
        assertNotNull(resC.getSimpleList());
        assertEquals(entityC.getSimpleList().size(), resC.getSimpleList().size());
        for (OWLClassA a : entityC.getSimpleList()) {
            final OWLClassA resA = em.find(OWLClassA.class, a.getUri(), listDescriptor);
            assertNotNull(resA);
            assertEquals(a.getUri(), resA.getUri());
            assertEquals(a.getStringAttribute(), resA.getStringAttribute());
        }
    }

    @Test
    public void testRetrieveReferencedListFromContext() throws Exception {
        logger.debug("Test: retrieve referenced list and its values from a different context.");
        this.em = getEntityManager("MultiRetrieveReferencedListFromContext", false);
        entityC.setReferencedList(Generators.createReferencedList(15));
        final Descriptor cDescriptor = new EntityDescriptor();
        final Descriptor listDescriptor = new ObjectPropertyCollectionDescriptor(CONTEXT_ONE,
                OWLClassC.class.getDeclaredField("referencedList"));
        cDescriptor.addAttributeDescriptor(OWLClassC.class.getDeclaredField("referencedList"), listDescriptor);
        em.getTransaction().begin();
        em.persist(entityC, cDescriptor);
        for (OWLClassA a : entityC.getReferencedList()) {
            em.persist(a, listDescriptor);
        }
        em.getTransaction().commit();

        final OWLClassC resC = em.find(OWLClassC.class, entityC.getUri(), cDescriptor);
        assertNotNull(resC);
        assertNotNull(resC.getReferencedList());
        assertEquals(entityC.getReferencedList().size(), resC.getReferencedList().size());
        for (OWLClassA a : entityC.getReferencedList()) {
            final OWLClassA resA = em.find(OWLClassA.class, a.getUri(), listDescriptor);
            assertNotNull(resA);
            assertEquals(a.getUri(), resA.getUri());
            assertEquals(a.getStringAttribute(), resA.getStringAttribute());
        }
    }

    @Test
    public void testRetrieveLazyReferenceFromContext() throws Exception {
        logger.debug("Test: retrieve entity with lazy loaded reference in another context.");
        this.em = getEntityManager("MultiRetrieveLazyReferenceFromContext", false);
        final Descriptor iDescriptor = new EntityDescriptor(CONTEXT_ONE);
        final Descriptor aDescriptor = new EntityDescriptor(CONTEXT_TWO);
        aDescriptor.addAttributeContext(OWLClassA.class.getDeclaredField("stringAttribute"), CONTEXT_ONE);
        iDescriptor.addAttributeDescriptor(OWLClassI.class.getDeclaredField("owlClassA"), aDescriptor);
        em.getTransaction().begin();
        // The relationship is CascadeType.PERSIST
        em.persist(entityI, iDescriptor);
        em.getTransaction().commit();

        final OWLClassI resI = em.find(OWLClassI.class, entityI.getUri(), iDescriptor);
        assertNotNull(resI);
        final Field refAField = OWLClassI.class.getDeclaredField("owlClassA");
        refAField.setAccessible(true);
        assertNull(refAField.get(resI));
        assertNotNull(resI.getOwlClassA());
        final OWLClassA resA = em.find(OWLClassA.class, entityA.getUri(), aDescriptor);
        assertNotNull(resA);
        // If we were using cache, ref.getOwlClassA() and resA would be the same
        assertEquals(resI.getOwlClassA().getStringAttribute(), resA.getStringAttribute());
    }

    @Test
    public void testRetrievePropertiesFromContext() throws Exception {
        logger.debug("Test: retrieve entity properties from a context.");
        this.em = getEntityManager("MultiRetrievePropertiesFromContext", false);
        entityB.setProperties(Generators.createProperties(50));
        final Descriptor bDescriptor = new EntityDescriptor(CONTEXT_ONE);
        bDescriptor.addAttributeContext(OWLClassB.class.getDeclaredField("properties"), CONTEXT_TWO);
        bDescriptor.addAttributeContext(OWLClassB.class.getDeclaredField("stringAttribute"), null);
        em.getTransaction().begin();
        em.persist(entityB, bDescriptor);
        em.getTransaction().commit();

        final OWLClassB res = em.find(OWLClassB.class, entityB.getUri(), bDescriptor);
        assertNotNull(res);
        assertEquals(entityB.getStringAttribute(), res.getStringAttribute());
        assertTrue(TestEnvironmentUtils.arePropertiesEqual(entityB.getProperties(), res.getProperties()));
    }
}

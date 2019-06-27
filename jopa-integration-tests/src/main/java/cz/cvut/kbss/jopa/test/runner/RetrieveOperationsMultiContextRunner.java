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

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.descriptors.ObjectPropertyCollectionDescriptor;
import cz.cvut.kbss.jopa.test.*;
import cz.cvut.kbss.jopa.test.environment.DataAccessor;
import cz.cvut.kbss.jopa.test.environment.Generators;
import cz.cvut.kbss.jopa.test.environment.PersistenceFactory;
import cz.cvut.kbss.jopa.test.environment.TestEnvironmentUtils;
import org.junit.Test;
import org.slf4j.Logger;

import java.lang.reflect.Field;

import static org.junit.Assert.*;

public abstract class RetrieveOperationsMultiContextRunner extends BaseRunner {

    public RetrieveOperationsMultiContextRunner(Logger logger, PersistenceFactory persistenceFactory,
                                                DataAccessor dataAccessor) {
        super(logger, persistenceFactory, dataAccessor);
    }

    @Test
    public void testRetrieveSimilarFromTwoContexts() {
        this.em = getEntityManager("MultiRetrieveSimilarFromTwoContexts", false);
        final OWLClassA entityATwo = new OWLClassA();
        entityATwo.setUri(entityA.getUri());
        entityATwo.setStringAttribute("SomeCompletelyDifferentStringAttribute");
        final Descriptor aDescriptor = new EntityDescriptor(CONTEXT_ONE);
        final Descriptor aTwoDescriptor = new EntityDescriptor(CONTEXT_TWO);
        em.getTransaction().begin();
        em.persist(entityA, aDescriptor);
        em.getTransaction().commit();
        em.getTransaction().begin();
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
        this.em = getEntityManager("MultiRetrieveSimpleListFromContext", false);
        entityC.setSimpleList(Generators.createSimpleList(10));
        final Descriptor cDescriptor = new EntityDescriptor();
        final ObjectPropertyCollectionDescriptor listDescriptor = new ObjectPropertyCollectionDescriptor(CONTEXT_ONE,
                OWLClassC.getSimpleListField(), false);
        cDescriptor.addAttributeDescriptor(OWLClassC.getSimpleListField(), listDescriptor);
        em.getTransaction().begin();
        em.persist(entityC, cDescriptor);
        for (OWLClassA a : entityC.getSimpleList()) {
            em.persist(a, listDescriptor.getElementDescriptor());
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
        this.em = getEntityManager("MultiRetrieveReferencedListFromContext", false);
        entityC.setReferencedList(Generators.createReferencedList(15));
        final Descriptor cDescriptor = new EntityDescriptor();
        final ObjectPropertyCollectionDescriptor listDescriptor = new ObjectPropertyCollectionDescriptor(CONTEXT_ONE,
                OWLClassC.getReferencedListField(), false);
        cDescriptor.addAttributeDescriptor(OWLClassC.getReferencedListField(), listDescriptor);
        em.getTransaction().begin();
        em.persist(entityC, cDescriptor);
        for (OWLClassA a : entityC.getReferencedList()) {
            em.persist(a, listDescriptor.getElementDescriptor());
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
        this.em = getEntityManager("MultiRetrieveLazyReferenceFromContext", false);
        final Descriptor iDescriptor = new EntityDescriptor(CONTEXT_ONE, false);
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

    @Test
    public void retrieveSupportsRetrievalOfReferenceWherePropertyAssertionIsInSubjectContext() throws Exception {
        this.em = getEntityManager("retrieveSupportsRetrievalOfReferenceWherePropertyAssertionIsInSubjectContext",
                false);
        final Descriptor dDescriptor = new EntityDescriptor(CONTEXT_ONE);
        final Descriptor aDescriptor = new EntityDescriptor(CONTEXT_TWO);
        dDescriptor.addAttributeDescriptor(OWLClassD.getOwlClassAField(), aDescriptor);
        em.getTransaction().begin();
        em.persist(entityA, aDescriptor);
        em.persist(entityD, dDescriptor);
        em.getTransaction().commit();

        final OWLClassD res = em.find(OWLClassD.class, entityD.getUri(), dDescriptor);
        assertNotNull(res);
        assertNotNull(res.getOwlClassA());
        assertEquals(entityA.getStringAttribute(), res.getOwlClassA().getStringAttribute());
    }
}

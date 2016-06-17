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
import cz.cvut.kbss.jopa.test.*;
import cz.cvut.kbss.jopa.test.environment.Generators;
import cz.cvut.kbss.jopa.test.environment.TestEnvironmentUtils;
import org.junit.Test;
import org.slf4j.Logger;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import static org.junit.Assert.*;

public abstract class UpdateOperationsMultiContextRunner extends BaseRunner {

    public UpdateOperationsMultiContextRunner(Logger logger) {
        super(logger);
    }

    @Test
    public void testUpdateDataPropertyInContext() throws Exception {
        logger.debug("Test: update data property value which is stored in a different context that the owner.");
        this.em = getEntityManager("MultiUpdateDataPropertyInContext", false);
        final Descriptor aDescriptor = new EntityDescriptor(CONTEXT_ONE);
        aDescriptor.addAttributeContext(OWLClassA.class.getDeclaredField("stringAttribute"), CONTEXT_TWO);
        em.getTransaction().begin();
        em.persist(entityA, aDescriptor);
        em.getTransaction().commit();

        em.getTransaction().begin();
        final OWLClassA a = em.find(OWLClassA.class, entityA.getUri(), aDescriptor);
        assertNotNull(a);
        final String newAttValue = "newStringAttributeValue";
        a.setStringAttribute(newAttValue);
        em.getTransaction().commit();

        final OWLClassA resA = em.find(OWLClassA.class, entityA.getUri(), aDescriptor);
        assertNotNull(resA);
        assertEquals(newAttValue, resA.getStringAttribute());
        assertEquals(entityA.getTypes(), resA.getTypes());
    }

    @Test
    public void testUpdateObjectPropertyToDifferentContext() throws Exception {
        logger.debug("Test: update object property with value from different context than the previous.");
        this.em = getEntityManager("MultiUpdateObjectPropertyToDifferent", false);
        final Descriptor dDescriptor = new EntityDescriptor();
        final Descriptor aDescriptor = new EntityDescriptor(CONTEXT_ONE);
        dDescriptor.addAttributeDescriptor(OWLClassD.class.getDeclaredField("owlClassA"), aDescriptor);
        em.getTransaction().begin();
        em.persist(entityD, dDescriptor);
        em.persist(entityA, aDescriptor);
        em.getTransaction().commit();

        final OWLClassD d = em.find(OWLClassD.class, entityD.getUri(), dDescriptor);
        assertNotNull(d);
        assertNotNull(d.getOwlClassA());
        em.getTransaction().begin();
        final OWLClassA newA = new OWLClassA();
        newA.setUri(URI.create("http://krizik.felk.cvut.cz/jopa/ontologies/newEntityA"));
        newA.setStringAttribute("newAStringAttribute");
        final Descriptor newADescriptor = new EntityDescriptor(CONTEXT_TWO);
        em.persist(newA, newADescriptor);
        dDescriptor.addAttributeDescriptor(OWLClassD.class.getDeclaredField("owlClassA"), newADescriptor);
        d.setOwlClassA(newA);
        em.getTransaction().commit();

        final OWLClassD resD = em.find(OWLClassD.class, entityD.getUri(), dDescriptor);
        assertNotNull(resD);
        assertEquals(newA.getUri(), resD.getOwlClassA().getUri());
        assertEquals(newA.getStringAttribute(), resD.getOwlClassA().getStringAttribute());
        final OWLClassA resA = em.find(OWLClassA.class, entityA.getUri(), aDescriptor);
        assertNotNull(resA);
        assertEquals(entityA.getStringAttribute(), resA.getStringAttribute());
    }

    @Test
    public void testUpdateAddToPropertiesInContext() throws Exception {
        logger.debug("Test: add new property value, properties are stored in a different context.");
        this.em = getEntityManager("MultiUpdateAddToPropertiesInContext", false);
        entityB.setProperties(Generators.createProperties());
        final Descriptor bDescriptor = new EntityDescriptor(CONTEXT_ONE);
        bDescriptor.addAttributeContext(OWLClassB.class.getDeclaredField("properties"), CONTEXT_TWO);
        em.getTransaction().begin();
        em.persist(entityB, bDescriptor);
        em.getTransaction().commit();

        em.getTransaction().begin();
        final OWLClassB b = em.find(OWLClassB.class, entityB.getUri(), bDescriptor);
        assertNotNull(b);
        final String newKey = "http://krizik.felk.cvut.cz/jopa/ontologies/properties/newPropertyKey";
        final String newValue = "http://krizik.felk.cvut.cz/jopa/ontologies/newPropertyValue";
        final String newPropertyValue = "http://krizik.felk.cvut.cz/jopa/ontologies/NewValueOfAnOldProperty";
        final String propertyToChange = b.getProperties().keySet().iterator().next();
        b.getProperties().put(newKey, Collections.singleton(newValue));
        b.getProperties().get(propertyToChange).add(newPropertyValue);
        em.getTransaction().commit();

        final OWLClassB res = em.find(OWLClassB.class, entityB.getUri(), bDescriptor);
        assertNotNull(res);
        assertEquals(entityB.getStringAttribute(), res.getStringAttribute());
        assertTrue(TestEnvironmentUtils.arePropertiesEqual(b.getProperties(), res.getProperties()));
    }

    @Test
    public void testUpdateAddToSimpleListInContext() throws Exception {
        logger.debug("Test: add new element into a simple list stored in different context than its owner.");
        this.em = getEntityManager("MultiUpdateAddToSimpleListInContext", false);
        entityC.setSimpleList(Generators.createSimpleList(15));
        final Descriptor cDescriptor = new EntityDescriptor(CONTEXT_ONE);
        final Descriptor lstDescriptor = new ObjectPropertyCollectionDescriptor(CONTEXT_TWO,
                OWLClassC.getSimpleListField());
        cDescriptor.addAttributeDescriptor(OWLClassC.getSimpleListField(), lstDescriptor);
        em.getTransaction().begin();
        em.persist(entityC, cDescriptor);
        entityC.getSimpleList().forEach(a -> em.persist(a, lstDescriptor));
        em.getTransaction().commit();

        em.getTransaction().begin();
        final OWLClassC c = em.find(OWLClassC.class, entityC.getUri(), cDescriptor);
        assertNotNull(c);
        assertEquals(entityC.getSimpleList().size(), c.getSimpleList().size());
        c.getSimpleList().add(entityA);
        em.persist(entityA, lstDescriptor);
        em.getTransaction().commit();

        final OWLClassC resC = em.find(OWLClassC.class, entityC.getUri(), cDescriptor);
        assertNotNull(resC);
        assertEquals(entityC.getSimpleList().size() + 1, resC.getSimpleList().size());
        boolean found = false;
        for (OWLClassA a : resC.getSimpleList()) {
            if (a.getUri().equals(entityA.getUri())) {
                assertEquals(entityA.getStringAttribute(), a.getStringAttribute());
                assertEquals(entityA.getTypes(), a.getTypes());
                found = true;
                break;
            }
        }
        assertTrue(found);
    }

    @Test
    public void testUpdateAddToReferencedListInContext() throws Exception {
        logger.debug("Test: add new element into a referenced list stored in different context than its owner.");
        this.em = getEntityManager("MultiUpdateAddToReferencedListInContext", false);
        entityC.setReferencedList(Generators.createReferencedList(10));
        final Descriptor cDescriptor = new EntityDescriptor(CONTEXT_ONE);
        final Descriptor lstDescriptor = new ObjectPropertyCollectionDescriptor(CONTEXT_TWO,
                OWLClassC.getReferencedListField());
        cDescriptor.addAttributeDescriptor(OWLClassC.getReferencedListField(), lstDescriptor);
        em.getTransaction().begin();
        em.persist(entityC, cDescriptor);
        entityC.getReferencedList().forEach(a -> em.persist(a, lstDescriptor));
        em.getTransaction().commit();

        em.getTransaction().begin();
        final OWLClassC c = em.find(OWLClassC.class, entityC.getUri(), cDescriptor);
        assertNotNull(c);
        assertEquals(entityC.getReferencedList().size(), c.getReferencedList().size());
        c.getReferencedList().add(entityA);
        em.persist(entityA, lstDescriptor);
        em.getTransaction().commit();

        final OWLClassC resC = em.find(OWLClassC.class, entityC.getUri(), cDescriptor);
        assertNotNull(resC);
        assertEquals(entityC.getReferencedList().size() + 1, resC.getReferencedList().size());
        boolean found = false;
        for (OWLClassA a : resC.getReferencedList()) {
            if (a.getUri().equals(entityA.getUri())) {
                assertEquals(entityA.getStringAttribute(), a.getStringAttribute());
                assertEquals(entityA.getTypes(), a.getTypes());
                found = true;
                break;
            }
        }
        assertTrue(found);
    }

    @Test
    public void testUpdateRemoveFromSimpleListInContext() throws Exception {
        logger.debug("Test: remove element from simple list stored in a different context than its owner.");
        this.em = getEntityManager("MultiUpdateRemoveFromSimpleListInContext", false);
        entityC.setSimpleList(Generators.createSimpleList(15));
        final Descriptor cDescriptor = new EntityDescriptor(CONTEXT_ONE);
        final Descriptor lstDescriptor = new ObjectPropertyCollectionDescriptor(CONTEXT_TWO,
                OWLClassC.getSimpleListField());
        cDescriptor.addAttributeDescriptor(OWLClassC.getSimpleListField(), lstDescriptor);
        em.getTransaction().begin();
        em.persist(entityC, cDescriptor);
        entityC.getSimpleList().forEach(a -> em.persist(a, lstDescriptor));
        em.getTransaction().commit();

        em.getTransaction().begin();
        final OWLClassC c = em.find(OWLClassC.class, entityC.getUri(), cDescriptor);
        assertNotNull(c);
        assertEquals(entityC.getSimpleList().size(), c.getSimpleList().size());
        final OWLClassA a = c.getSimpleList().get(0);
        c.getSimpleList().remove(0);
        em.remove(a);
        em.getTransaction().commit();

        final OWLClassC resC = em.find(OWLClassC.class, entityC.getUri(), cDescriptor);
        assertNotNull(resC);
        assertEquals(entityC.getSimpleList().size() - 1, resC.getSimpleList().size());
        assertNull(em.find(OWLClassA.class, a.getUri(), lstDescriptor));
    }

    @Test
    public void testUpdateRemoveFromReferencedListInContext() throws Exception {
        logger.debug("Test: remove elements from referenced list stored in a different context than its owner.");
        this.em = getEntityManager("MultiUpdateRemoveFromReferencedListInContext", false);
        entityC.setReferencedList(Generators.createReferencedList(10));
        final Descriptor cDescriptor = new EntityDescriptor(CONTEXT_ONE);
        final Descriptor lstDescriptor = new ObjectPropertyCollectionDescriptor(CONTEXT_TWO,
                OWLClassC.getReferencedListField());
        cDescriptor.addAttributeDescriptor(OWLClassC.getReferencedListField(), lstDescriptor);
        em.getTransaction().begin();
        em.persist(entityC, cDescriptor);
        entityC.getReferencedList().forEach(a -> em.persist(a, lstDescriptor));
        em.getTransaction().commit();

        em.getTransaction().begin();
        final OWLClassC c = em.find(OWLClassC.class, entityC.getUri(), cDescriptor);
        assertNotNull(c);
        assertEquals(entityC.getReferencedList().size(), c.getReferencedList().size());
        final List<OWLClassA> removed = new ArrayList<>();
        int i = 0;
        final Iterator<OWLClassA> it = c.getReferencedList().iterator();
        while (it.hasNext()) {
            i++;
            final OWLClassA a = it.next();
            if (i % 2 == 1) {
                continue;
            }
            removed.add(a);
            it.remove();
        }
        em.getTransaction().commit();

        final OWLClassC resC = em.find(OWLClassC.class, entityC.getUri(), cDescriptor);
        assertNotNull(resC);
        assertEquals(entityC.getReferencedList().size() - removed.size(), resC.getReferencedList()
                                                                              .size());
        for (OWLClassA a : removed) {
            final OWLClassA resA = em.find(OWLClassA.class, a.getUri(), lstDescriptor);
            assertNotNull(resA);
        }
    }

    @Test
    public void testUpdatePlainIdentifierObjectPropertyValueInContext() {
        final Descriptor pDescriptor = new EntityDescriptor(CONTEXT_ONE);
        entityP.setIndividualUri(URI.create("http://krizik.felk.cvut.cz/originalIndividual"));
        this.em = getEntityManager("UpdatePlainIdentifierObjectPropertyValueInContext", true);
        em.getTransaction().begin();
        em.persist(entityP, pDescriptor);
        em.getTransaction().commit();

        final OWLClassP toUpdate = em.find(OWLClassP.class, entityP.getUri());
        em.detach(toUpdate);
        final URI newUri = URI.create("http://krizik.felk.cvut.cz/newIndividual");
        toUpdate.setIndividualUri(newUri);
        em.getTransaction().begin();
        em.merge(toUpdate);
        em.getTransaction().commit();

        final OWLClassP res = em.find(OWLClassP.class, entityP.getUri());
        assertNotNull(res);
        assertEquals(newUri, res.getIndividualUri());
    }

    @Test
    public void testUpdateFieldInMappedSuperclassInContext() throws Exception {
        final Descriptor qDescriptor = new EntityDescriptor(CONTEXT_ONE);
        final Descriptor aDescriptor = new EntityDescriptor(CONTEXT_TWO);
        qDescriptor.addAttributeDescriptor(OWLClassQ.getOWlClassAField(), aDescriptor);
        this.em = getEntityManager("UpdateFieldInMappedSuperclassInContext", true);
        em.getTransaction().begin();
        em.persist(entityQ, qDescriptor);
        em.persist(entityA, aDescriptor);
        em.getTransaction().commit();

        entityQ.setStringAttribute("newStringAttribute");
        entityQ.setParentString("newParentStringAttribute");
        entityQ.setLabel("newLabel");
        final OWLClassA newA = new OWLClassA(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityA2"));
        newA.setStringAttribute("newAString");
        entityQ.setOwlClassA(newA);
        em.getTransaction().begin();
        em.merge(entityQ, qDescriptor);
        em.persist(newA, aDescriptor);
        em.getTransaction().commit();

        final OWLClassQ res = em.find(OWLClassQ.class, entityQ.getUri(), qDescriptor);
        assertNotNull(res);
        assertEquals(entityQ.getStringAttribute(), res.getStringAttribute());
        assertEquals(entityQ.getParentString(), res.getParentString());
        assertEquals(entityQ.getLabel(), res.getLabel());
        assertNotNull(res.getOwlClassA());
        assertEquals(newA.getUri(), res.getOwlClassA().getUri());
        assertNotNull(em.find(OWLClassA.class, newA.getUri()));
        assertNotNull(em.find(OWLClassA.class, entityA.getUri(), aDescriptor));
    }
}

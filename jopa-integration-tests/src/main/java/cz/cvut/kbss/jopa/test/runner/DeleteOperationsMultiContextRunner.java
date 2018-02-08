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

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.OWLClassD;
import cz.cvut.kbss.jopa.test.OWLClassG;
import cz.cvut.kbss.jopa.test.OWLClassH;
import org.junit.Test;
import org.slf4j.Logger;

import static org.junit.Assert.*;

public abstract class DeleteOperationsMultiContextRunner extends BaseRunner {

    public DeleteOperationsMultiContextRunner(Logger logger) {
        super(logger);
    }

    @Test
    public void testRemoveFromContext() {
        logger.debug("Test: remove entity from a context.");
        this.em = getEntityManager("MultiRemoveFromContext", false);
        final Descriptor aDescriptor = new EntityDescriptor(CONTEXT_ONE);
        em.getTransaction().begin();
        em.persist(entityA, aDescriptor);
        em.getTransaction().commit();

        final OWLClassA a = em.find(OWLClassA.class, entityA.getUri(), aDescriptor);
        assertNotNull(a);
        em.getTransaction().begin();
        em.remove(a);
        assertFalse(em.contains(a));
        em.getTransaction().commit();

        final OWLClassA res = em.find(OWLClassA.class, entityA.getUri(), aDescriptor);
        assertNull(res);
    }

    @Test
    public void testRemoveFromOneKeepInTheOther() {
        logger.debug("Test: persist an entity into two contexts and then remove it from one of them.");
        this.em = getEntityManager("MultiRemoveFromOneContextAndKeepInTheOther", false);
        final Descriptor aDescriptorOne = new EntityDescriptor(CONTEXT_ONE);
        final Descriptor aDescriptorTwo = new EntityDescriptor(CONTEXT_TWO);
        em.getTransaction().begin();
        em.persist(entityA, aDescriptorOne);
        em.getTransaction().commit();
        em.getTransaction().begin();
        em.persist(entityA, aDescriptorTwo);
        em.getTransaction().commit();

        final OWLClassA aOne = em.find(OWLClassA.class, entityA.getUri(), aDescriptorOne);
        assertNotNull(aOne);
        final OWLClassA aTwo = em.find(OWLClassA.class, entityA.getUri(), aDescriptorTwo);
        assertNotNull(aTwo);
        em.getTransaction().begin();
        em.remove(aTwo);
        em.getTransaction().commit();

        final OWLClassA resOne = em.find(OWLClassA.class, entityA.getUri(), aDescriptorOne);
        assertNotNull(resOne);
        final OWLClassA resTwo = em.find(OWLClassA.class, entityA.getUri(), aDescriptorTwo);
        assertNull(resTwo);
    }

    @Test
    public void testRemoveObjectPropertyFromContext() throws Exception {
        logger.debug("Test: remove object property value from a context.");
        this.em = getEntityManager("MultiRemoveObjectPropertyFromContext", false);
        final Descriptor dDescriptor = new EntityDescriptor(CONTEXT_ONE);
        final Descriptor aDescriptor = new EntityDescriptor(CONTEXT_TWO);
        dDescriptor.addAttributeDescriptor(OWLClassD.class.getDeclaredField("owlClassA"), aDescriptor);
        em.getTransaction().begin();
        em.persist(entityD, dDescriptor);
        em.persist(entityA, aDescriptor);
        em.getTransaction().commit();

        final OWLClassD d = em.find(OWLClassD.class, entityD.getUri(), dDescriptor);
        assertNotNull(d);
        final OWLClassA a = d.getOwlClassA();
        assertNotNull(a);
        d.setOwlClassA(null);
        em.getTransaction().begin();
        em.remove(a);
        em.getTransaction().commit();

        final OWLClassD resD = em.find(OWLClassD.class, entityD.getUri(), dDescriptor);
        assertNotNull(resD);
        assertNull(resD.getOwlClassA());
        final OWLClassA resA = em.find(OWLClassA.class, entityA.getUri(), aDescriptor);
        assertNull(resA);
    }

    @Test
    public void testRemoveCascadeOverContexts() throws Exception {
        logger.debug("Test: remove entities through cascade, each in a different context.");
        this.em = getEntityManager("MultiRemoveCascadeOverContexts", false);
        final Descriptor gDescriptor = new EntityDescriptor();
        final Descriptor hDescriptor = new EntityDescriptor(CONTEXT_ONE);
        final Descriptor aDescriptor = new EntityDescriptor(CONTEXT_TWO);
        hDescriptor.addAttributeDescriptor(OWLClassH.class.getDeclaredField("owlClassA"), aDescriptor);
        gDescriptor.addAttributeDescriptor(OWLClassG.class.getDeclaredField("owlClassH"), hDescriptor);
        em.getTransaction().begin();
        em.persist(entityG, gDescriptor);
        em.getTransaction().commit();

        final OWLClassA a = em.find(OWLClassA.class, entityA.getUri(), aDescriptor);
        assertNotNull(a);
        final OWLClassH h = em.find(OWLClassH.class, entityH.getUri(), hDescriptor);
        assertNotNull(h);
        assertSame(a, h.getOwlClassA());
        final OWLClassG g = em.find(OWLClassG.class, entityG.getUri(), gDescriptor);
        assertNotNull(g);
        assertSame(h, g.getOwlClassH());
        em.getTransaction().begin();
        em.remove(g);
        em.getTransaction().commit();

        assertNull(em.find(OWLClassA.class, entityA.getUri(), aDescriptor));
        assertNull(em.find(OWLClassH.class, entityH.getUri(), hDescriptor));
        assertNull(em.find(OWLClassG.class, entityG.getUri(), gDescriptor));
    }
}

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

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.test.*;
import cz.cvut.kbss.jopa.test.environment.DataAccessor;
import cz.cvut.kbss.jopa.test.environment.PersistenceFactory;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;

import java.net.URI;

import static org.junit.jupiter.api.Assertions.*;

public abstract class DeleteOperationsMultiContextRunner extends BaseRunner {

    public DeleteOperationsMultiContextRunner(Logger logger, PersistenceFactory persistenceFactory,
                                              DataAccessor dataAccessor) {
        super(logger, persistenceFactory, dataAccessor);
    }

    @Test
    void testRemoveFromContext() {
        this.em = getEntityManager("MultiRemoveFromContext", false);
        final Descriptor aDescriptor = new EntityDescriptor(CONTEXT_ONE);
        em.getTransaction().begin();
        em.persist(entityA, aDescriptor);
        em.getTransaction().commit();

        final OWLClassA a = findRequired(OWLClassA.class, entityA.getUri(), aDescriptor);
        em.getTransaction().begin();
        em.remove(a);
        assertFalse(em.contains(a));
        em.getTransaction().commit();

        final OWLClassA res = em.find(OWLClassA.class, entityA.getUri(), aDescriptor);
        assertNull(res);
    }

    @Test
    void testRemoveFromOneKeepInTheOther() {
        this.em = getEntityManager("MultiRemoveFromOneContextAndKeepInTheOther", false);
        final Descriptor aDescriptorOne = new EntityDescriptor(CONTEXT_ONE);
        final Descriptor aDescriptorTwo = new EntityDescriptor(CONTEXT_TWO);
        em.getTransaction().begin();
        em.persist(entityA, aDescriptorOne);
        em.getTransaction().commit();
        em.getTransaction().begin();
        em.persist(entityA, aDescriptorTwo);
        em.getTransaction().commit();

        findRequired(OWLClassA.class, entityA.getUri(), aDescriptorOne);
        final OWLClassA aTwo = findRequired(OWLClassA.class, entityA.getUri(), aDescriptorTwo);
        em.getTransaction().begin();
        em.remove(aTwo);
        em.getTransaction().commit();

        findRequired(OWLClassA.class, entityA.getUri(), aDescriptorOne);
        final OWLClassA resTwo = em.find(OWLClassA.class, entityA.getUri(), aDescriptorTwo);
        assertNull(resTwo);
    }

    @Test
    void testRemoveObjectPropertyFromContext() throws Exception {
        this.em = getEntityManager("MultiRemoveObjectPropertyFromContext", false);
        final Descriptor dDescriptor = new EntityDescriptor(CONTEXT_ONE);
        final Descriptor aDescriptor = new EntityDescriptor(CONTEXT_TWO);
        dDescriptor.addAttributeDescriptor(OWLClassD.getOwlClassAField(), aDescriptor);
        em.getTransaction().begin();
        em.persist(entityD, dDescriptor);
        em.persist(entityA, aDescriptor);
        em.getTransaction().commit();

        final OWLClassD d = findRequired(OWLClassD.class, entityD.getUri(), dDescriptor);
        final OWLClassA a = d.getOwlClassA();
        assertNotNull(a);
        d.setOwlClassA(null);
        em.getTransaction().begin();
        em.remove(a);
        em.getTransaction().commit();

        final OWLClassD resD = findRequired(OWLClassD.class, entityD.getUri(), dDescriptor);
        assertNull(resD.getOwlClassA());
        final OWLClassA resA = em.find(OWLClassA.class, entityA.getUri(), aDescriptor);
        assertNull(resA);
    }

    @Test
    void testRemoveCascadeOverContexts() throws Exception {
        this.em = getEntityManager("MultiRemoveCascadeOverContexts", false);
        final Descriptor gDescriptor = new EntityDescriptor();
        final Descriptor hDescriptor = new EntityDescriptor(CONTEXT_ONE);
        final Descriptor aDescriptor = new EntityDescriptor(CONTEXT_TWO);
        hDescriptor.addAttributeDescriptor(OWLClassH.class.getDeclaredField("owlClassA"), aDescriptor);
        gDescriptor.addAttributeDescriptor(OWLClassG.class.getDeclaredField("owlClassH"), hDescriptor);
        em.getTransaction().begin();
        em.persist(entityG, gDescriptor);
        em.getTransaction().commit();

        final OWLClassA a = findRequired(OWLClassA.class, entityA.getUri(), aDescriptor);
        final OWLClassH h = findRequired(OWLClassH.class, entityH.getUri(), hDescriptor);
        assertSame(a, h.getOwlClassA());
        final OWLClassG g = findRequired(OWLClassG.class, entityG.getUri(), gDescriptor);
        assertSame(h, g.getOwlClassH());
        em.getTransaction().begin();
        em.remove(g);
        em.getTransaction().commit();

        assertNull(em.find(OWLClassA.class, entityA.getUri(), aDescriptor));
        assertNull(em.find(OWLClassH.class, entityH.getUri(), hDescriptor));
        assertNull(em.find(OWLClassG.class, entityG.getUri(), gDescriptor));
    }

    @Test
    void removeRemovesObjectPropertyAssertionFromTargetContext() throws Exception {
        this.em = getEntityManager("removeRemovesObjectPropertyAssertionFromTargetContext", false);
        final Descriptor dDescriptor = new EntityDescriptor(CONTEXT_ONE, false);
        final Descriptor aDescriptor = new EntityDescriptor(CONTEXT_TWO);
        dDescriptor.addAttributeDescriptor(OWLClassD.getOwlClassAField(), aDescriptor);
        transactional(() -> {
            em.persist(entityD, dDescriptor);
            em.persist(entityA, aDescriptor);
        });

        transactional(() -> {
            final OWLClassD d = findRequired(OWLClassD.class, entityD.getUri(), dDescriptor);
            d.setOwlClassA(null);
        });

        assertFalse(em.createNativeQuery("ASK { ?s ?p ?o . }", Boolean.class)
                      .setParameter("p", entityD.getUri())
                      .setParameter("p", URI.create(Vocabulary.P_HAS_OWL_CLASS_A))
                      .setParameter("o", entityA.getUri())
                      .getSingleResult());
        assertNotNull(em.find(OWLClassA.class, entityA.getUri(), aDescriptor));
    }
}

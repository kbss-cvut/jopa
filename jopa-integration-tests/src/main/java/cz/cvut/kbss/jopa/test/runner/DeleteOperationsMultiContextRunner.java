/**
 * Copyright (C) 2023 Czech Technical University in Prague
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
        transactional(() -> em.persist(entityA, aDescriptor));

        final OWLClassA a = findRequired(OWLClassA.class, entityA.getUri(), aDescriptor);
        transactional(() -> {
            em.remove(a);
            assertFalse(em.contains(a));
        });

        final OWLClassA res = em.find(OWLClassA.class, entityA.getUri(), aDescriptor);
        assertNull(res);
    }

    @Test
    void testRemoveFromOneKeepInTheOther() {
        this.em = getEntityManager("MultiRemoveFromOneContextAndKeepInTheOther", false);
        final Descriptor aDescriptorOne = new EntityDescriptor(CONTEXT_ONE);
        final Descriptor aDescriptorTwo = new EntityDescriptor(CONTEXT_TWO);
        transactional(() -> em.persist(entityA, aDescriptorOne));
        transactional(() -> em.persist(entityA, aDescriptorTwo));

        findRequired(OWLClassA.class, entityA.getUri(), aDescriptorOne);
        final OWLClassA aTwo = findRequired(OWLClassA.class, entityA.getUri(), aDescriptorTwo);
        transactional(() -> em.remove(aTwo));

        findRequired(OWLClassA.class, entityA.getUri(), aDescriptorOne);
        final OWLClassA resTwo = em.find(OWLClassA.class, entityA.getUri(), aDescriptorTwo);
        assertNull(resTwo);
    }

    @Test
    void testRemoveObjectPropertyFromContext() {
        this.em = getEntityManager("MultiRemoveObjectPropertyFromContext", false);
        final Descriptor dDescriptor = new EntityDescriptor(CONTEXT_ONE);
        final Descriptor aDescriptor = new EntityDescriptor(CONTEXT_TWO);
        dDescriptor.addAttributeDescriptor(fieldSpecification(OWLClassD.class, "owlClassA"), aDescriptor);
        transactional(() -> {
            em.persist(entityD, dDescriptor);
            em.persist(entityA, aDescriptor);
        });

        final OWLClassD d = findRequired(OWLClassD.class, entityD.getUri(), dDescriptor);
        final OWLClassA a = d.getOwlClassA();
        assertNotNull(a);
        d.setOwlClassA(null);
        transactional(() -> em.remove(a));

        final OWLClassD resD = findRequired(OWLClassD.class, entityD.getUri(), dDescriptor);
        assertNull(resD.getOwlClassA());
        final OWLClassA resA = em.find(OWLClassA.class, entityA.getUri(), aDescriptor);
        assertNull(resA);
    }

    @Test
    void testRemoveCascadeOverContexts() {
        this.em = getEntityManager("MultiRemoveCascadeOverContexts", false);
        final Descriptor gDescriptor = new EntityDescriptor();
        final Descriptor hDescriptor = new EntityDescriptor(CONTEXT_ONE);
        final Descriptor aDescriptor = new EntityDescriptor(CONTEXT_TWO);
        hDescriptor.addAttributeDescriptor(fieldSpecification(OWLClassH.class, "owlClassA"), aDescriptor);
        gDescriptor.addAttributeDescriptor(fieldSpecification(OWLClassG.class, "owlClassH"), hDescriptor);
        transactional(() -> em.persist(entityG, gDescriptor));

        final OWLClassA a = findRequired(OWLClassA.class, entityA.getUri(), aDescriptor);
        final OWLClassH h = findRequired(OWLClassH.class, entityH.getUri(), hDescriptor);
        assertSame(a, h.getOwlClassA());
        final OWLClassG g = findRequired(OWLClassG.class, entityG.getUri(), gDescriptor);
        assertSame(h, g.getOwlClassH());
        transactional(() -> em.remove(g));

        assertNull(em.find(OWLClassA.class, entityA.getUri(), aDescriptor));
        assertNull(em.find(OWLClassH.class, entityH.getUri(), hDescriptor));
        assertNull(em.find(OWLClassG.class, entityG.getUri(), gDescriptor));
    }

    @Test
    void removeRemovesObjectPropertyAssertionFromTargetContext() {
        this.em = getEntityManager("removeRemovesObjectPropertyAssertionFromTargetContext", false);
        final Descriptor dDescriptor = new EntityDescriptor(CONTEXT_ONE, false);
        final Descriptor aDescriptor = new EntityDescriptor(CONTEXT_TWO);
        dDescriptor.addAttributeDescriptor(fieldSpecification(OWLClassD.class, "owlClassA"), aDescriptor);
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

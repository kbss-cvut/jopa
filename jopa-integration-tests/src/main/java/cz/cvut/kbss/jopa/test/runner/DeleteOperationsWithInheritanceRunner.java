/*
 * JOPA
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */
package cz.cvut.kbss.jopa.test.runner;

import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.OWLClassQ;
import cz.cvut.kbss.jopa.test.OWLClassT;
import cz.cvut.kbss.jopa.test.environment.DataAccessor;
import cz.cvut.kbss.jopa.test.environment.PersistenceFactory;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;

import static org.junit.jupiter.api.Assertions.*;

public abstract class DeleteOperationsWithInheritanceRunner extends BaseInheritanceRunner {

    public DeleteOperationsWithInheritanceRunner(Logger logger, PersistenceFactory persistenceFactory,
                                                 DataAccessor dataAccessor) {
        super(logger, persistenceFactory, dataAccessor);
    }

    @Test
    public void testRemoveEntityWithMappedSuperclass() {
        this.em = getEntityManager("RemoveEntityWithMappedSuperclass", false);
        persist(entityQ, entityA);

        em.getTransaction().begin();
        final OWLClassQ toRemove = em.find(OWLClassQ.class, entityQ.getUri());
        assertNotNull(toRemove);
        em.remove(toRemove);
        em.getTransaction().commit();

        assertNull(em.find(OWLClassQ.class, entityQ.getUri()));
        assertNotNull(em.find(OWLClassA.class, entityA.getUri()));
        verifyIndividualWasRemoved(entityQ.getUri());
    }

    @Test
    public void testRemoveEntityWithEntitySuperclass() {
        this.em = getEntityManager("RemoveEntityWithEntitySuperclass", false);
        persist(entityT, entityA);

        em.getTransaction().begin();
        final OWLClassT toRemove = em.find(OWLClassT.class, entityT.getUri());
        assertNotNull(toRemove);
        em.remove(toRemove);
        assertFalse(em.contains(toRemove));
        em.getTransaction().commit();

        assertNull(em.find(OWLClassT.class, entityT.getUri()));
        assertNotNull(em.find(OWLClassA.class, entityA.getUri()));
        verifyIndividualWasRemoved(entityT.getUri());
    }
}

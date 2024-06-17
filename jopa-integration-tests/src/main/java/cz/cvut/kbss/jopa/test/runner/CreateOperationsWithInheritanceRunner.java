/*
 * JOPA
 * Copyright (C) 2024 Czech Technical University in Prague
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

import cz.cvut.kbss.jopa.test.OWLClassQ;
import cz.cvut.kbss.jopa.test.OWLClassS;
import cz.cvut.kbss.jopa.test.OWLClassT;
import cz.cvut.kbss.jopa.test.environment.DataAccessor;
import cz.cvut.kbss.jopa.test.environment.PersistenceFactory;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;

import static org.junit.jupiter.api.Assertions.*;

public abstract class CreateOperationsWithInheritanceRunner extends BaseInheritanceRunner {

    protected CreateOperationsWithInheritanceRunner(Logger logger, PersistenceFactory persistenceFactory,
                                                    DataAccessor dataAccessor) {
        super(logger, persistenceFactory, dataAccessor);
    }

    @Test
    void testPersistEntityWithMappedSuperclass() {
        this.em = getEntityManager("PersistEntityWithMappedSuperclass", false);
        em.getTransaction().begin();
        em.persist(entityA);
        em.persist(entityQ);
        em.getTransaction().commit();
        em.clear();

        assertNotNull(entityQ.getUri());
        final OWLClassQ result = em.find(OWLClassQ.class, entityQ.getUri());
        assertNotNull(result);
        assertEquals(entityQ.getStringAttribute(), result.getStringAttribute());
        assertEquals(entityQ.getParentString(), result.getParentString());
        assertEquals(entityQ.getLabel(), result.getLabel());
        assertNotNull(result.getOwlClassA());
        assertEquals(entityQ.getOwlClassA().getUri(), result.getOwlClassA().getUri());
        assertEquals(entityA.getStringAttribute(), result.getOwlClassA().getStringAttribute());
        assertEquals(entityA.getTypes(), result.getOwlClassA().getTypes());
    }

    @Test
    void testPersistEntityWithEntitySuperclass() {
        this.em = getEntityManager("PersistEntityWithEntitySuperclass", false);
        em.getTransaction().begin();
        em.persist(entityA);
        em.persist(entityT);
        em.getTransaction().commit();
        em.clear();

        assertNotNull(entityT.getUri());
        final OWLClassT result = em.find(OWLClassT.class, entityT.getUri());
        assertNotNull(result);
        assertEquals(entityT.getName(), result.getName());
        assertEquals(entityT.getDescription(), result.getDescription());
        assertEquals(entityT.getIntAttribute(), result.getIntAttribute());
        assertNotNull(result.getOwlClassA());
        assertEquals(entityA.getUri(), result.getOwlClassA().getUri());
        assertEquals(entityA.getStringAttribute(), result.getOwlClassA().getStringAttribute());
        assertEquals(entityA.getTypes(), result.getOwlClassA().getTypes());
    }

    @Test
    void testPersistEntityExtendedBySubEntity() {
        this.em = getEntityManager("PersistEntityExtendedBySubEntity", false);
        final OWLClassS entityS = new OWLClassS();
        entityS.setName("Supertype");
        entityS.setDescription("Supertype is a type with a subclass.");
        em.getTransaction().begin();
        em.persist(entityS);
        em.getTransaction().commit();
        em.clear();

        assertNotNull(entityS.getUri());
        final OWLClassS resultS = em.find(OWLClassS.class, entityS.getUri());
        assertNotNull(resultS);
        assertEquals(entityS.getName(), resultS.getName());
        assertEquals(entityS.getDescription(), resultS.getDescription());
        em.clear();
        assertNull(em.find(OWLClassT.class, entityS.getUri()));
    }
}

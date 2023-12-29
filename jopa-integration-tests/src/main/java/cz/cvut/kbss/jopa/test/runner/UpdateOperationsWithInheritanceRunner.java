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

import cz.cvut.kbss.jopa.exceptions.IntegrityConstraintViolatedException;
import cz.cvut.kbss.jopa.exceptions.RollbackException;
import cz.cvut.kbss.jopa.test.*;
import cz.cvut.kbss.jopa.test.environment.DataAccessor;
import cz.cvut.kbss.jopa.test.environment.Generators;
import cz.cvut.kbss.jopa.test.environment.PersistenceFactory;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.instanceOf;
import static org.junit.jupiter.api.Assertions.*;

public abstract class UpdateOperationsWithInheritanceRunner extends BaseInheritanceRunner {

    public UpdateOperationsWithInheritanceRunner(Logger logger, PersistenceFactory persistenceFactory,
                                                 DataAccessor dataAccessor) {
        super(logger, persistenceFactory, dataAccessor);
    }

    @Test
    void testUpdateFieldsOfEntityWithMappedSuperclass() {
        this.em = getEntityManager("UpdateEntityWithMappedSuperclass", true);
        persist(entityQ, entityA);

        entityQ.setStringAttribute("newStringAttribute");
        entityQ.setParentString("newParentStringAttribute");
        entityQ.setLabel("newLabel");
        em.getTransaction().begin();
        em.merge(entityQ);
        em.getTransaction().commit();

        final OWLClassQ res = findRequired(OWLClassQ.class, entityQ.getUri());
        assertEquals(entityQ.getStringAttribute(), res.getStringAttribute());
        assertEquals(entityQ.getParentString(), res.getParentString());
        assertEquals(entityQ.getLabel(), res.getLabel());
    }

    @Test
    void testUpdateObjectPropertyInMappedSuperclass() {
        this.em = getEntityManager("UpdateObjectPropertyInMappedSuperclass", true);
        persist(entityQ, entityA);
        final OWLClassA entityA2 = new OWLClassA(Generators.generateUri());
        entityA2.setStringAttribute("entityA2StringAttribute");

        entityQ.setOwlClassA(entityA2);
        em.getTransaction().begin();
        em.merge(entityQ);
        em.persist(entityA2);
        em.getTransaction().commit();

        final OWLClassQ res = findRequired(OWLClassQ.class, entityQ.getUri());
        assertNotNull(res.getOwlClassA());
        assertEquals(entityA2.getUri(), res.getOwlClassA().getUri());
        assertEquals(entityA2.getStringAttribute(), res.getOwlClassA().getStringAttribute());
        assertNotNull(em.find(OWLClassA.class, entityA.getUri()));
    }

    @Test
    void settingNonEmptyFieldInMappedSuperclassThrowsICViolationOnMerge() {
        this.em = getEntityManager("SettingNonEmptyFieldInMappedSuperclassThrowsICViolation", true);
        persist(entityQ, entityA);

        final RollbackException ex = assertThrows(RollbackException.class, () -> {
            entityQ.setOwlClassA(null);
            em.getTransaction().begin();
            em.merge(entityQ);
            em.getTransaction().commit();
        });
        assertThat(ex.getCause(), instanceOf(IntegrityConstraintViolatedException.class));

    }

    @Test
    void testUpdateDataPropertyInEntitySuperclass() {
        this.em = getEntityManager("updateDataPropertyInEntitySuperclass", true);
        persist(entityT, entityA);

        final String newName = "newName";
        final int newInt = Generators.randomInt(Integer.MAX_VALUE);
        entityT.setName(newName);
        entityT.setIntAttribute(newInt);
        final String newDescription = "new entity description";
        em.getTransaction().begin();
        final OWLClassT merged = em.merge(entityT);
        merged.setDescription(newDescription);
        em.getTransaction().commit();

        final OWLClassT result = findRequired(OWLClassT.class, entityT.getUri());
        assertEquals(newName, result.getName());
        assertEquals(newDescription, result.getDescription());
        assertEquals(newInt, result.getIntAttribute().intValue());
    }

    @Test
    void updateAllowsSettingValueOfPolymorphicAttributeToInstanceOfDifferentSubtype() {
        this.em = getEntityManager("updateAllowsSettingValueOfPolymorphicAttributeToInstanceOfDifferentSubtype", true);
        persist(entityU, entityT, entityA);

        final OWLClassU newReference = new OWLClassU();
        newReference.setName("UpdatedU");
        newReference.setDescription("Description");

        em.getTransaction().begin();
        em.persist(newReference);
        final OWLClassU toUpdate = findRequired(OWLClassU.class, entityU.getUri());
        toUpdate.setOwlClassS(newReference);
        em.getTransaction().commit();

        final OWLClassU result = findRequired(OWLClassU.class, entityU.getUri());
        assertTrue(result.getOwlClassS() instanceof OWLClassU);
        assertEquals(newReference.getUri(), result.getOwlClassS().getUri());
        assertNotNull(em.find(OWLClassS.class, entityT.getUri()));
    }
}

/*
 * JOPA
 * Copyright (C) 2025 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.test.query.runner;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.test.OWLClassS;
import cz.cvut.kbss.jopa.test.OWLClassSParent;
import cz.cvut.kbss.jopa.test.OWLClassT;
import cz.cvut.kbss.jopa.test.Vocabulary;
import cz.cvut.kbss.jopa.test.environment.DataAccessor;
import cz.cvut.kbss.jopa.test.environment.Generators;
import cz.cvut.kbss.jopa.test.query.QueryTestEnvironment;
import cz.cvut.kbss.jopa.vocabulary.RDFS;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;

import java.net.URI;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;


public abstract class PolymorphicSelectQueryRunner extends BaseQueryRunner {

    protected PolymorphicSelectQueryRunner(Logger logger, DataAccessor dataAccessor) {
        super(logger, dataAccessor);
    }

    @AfterEach
    public void tearDown() {
        getEntityManager().clear();
        getEntityManager().getEntityManagerFactory().getCache().evictAll();
    }

    @Test
    public void selectLoadsInstanceOfMostConcreteSubclassOfAbstractEntity() {
        final OWLClassT t = Generators.getRandomItem(QueryTestEnvironment.getData(OWLClassT.class));
        final EntityManager em = getEntityManager();
        final OWLClassSParent result =
                em.createNativeQuery("SELECT ?x WHERE { ?x ?hasName ?name . }", OWLClassSParent.class)
                  .setParameter("hasName", URI.create(RDFS.LABEL))
                  .setParameter("name", t.getName(), "en").getSingleResult();
        assertNotNull(result);
        assertTrue(result instanceof OWLClassT);
        verifyEntityTAttributes(t, (OWLClassT) result);
    }

    private static void verifyEntityTAttributes(OWLClassT expected, OWLClassT actual) {
        assertEquals(expected.getUri(), actual.getUri());
        assertEquals(expected.getName(), actual.getName());
        assertEquals(expected.getDescription(), actual.getDescription());
        assertEquals(expected.getIntAttribute(), actual.getIntAttribute());
        assertEquals(expected.getOwlClassA().getUri(), actual.getOwlClassA().getUri());
    }

    @Test
    public void selectLoadsInstanceOfMostConcreteSubclassOfConcreteEntity() {
        final OWLClassT t = Generators.getRandomItem(QueryTestEnvironment.getData(OWLClassT.class));
        final EntityManager em = getEntityManager();
        final OWLClassS result =
                em.createNativeQuery("SELECT ?x WHERE { ?x ?hasName ?name . }", OWLClassS.class)
                  .setParameter("hasName", URI.create(RDFS.LABEL))
                  .setParameter("name", t.getName(), "en").getSingleResult();
        assertNotNull(result);
        assertTrue(result instanceof OWLClassT);
        verifyEntityTAttributes(t, (OWLClassT) result);
    }

    @Test
    void selectByTypeLoadsAllIndividualsAsMostConcreteSubclassInstances() {
        final List<OWLClassT> data = QueryTestEnvironment.getData(OWLClassT.class);
        final EntityManager em = getEntityManager();
        // This will cause the type resolver to have to do some work
        em.getTransaction().begin();
        data.forEach(t -> {
            t.setTypes(new HashSet<>(Arrays.asList(Vocabulary.C_OWL_CLASS_S_PARENT, Vocabulary.C_OWL_CLASS_S)));
            em.merge(t);
        });
        em.getTransaction().commit();
        final List<OWLClassSParent> result =
                em.createNativeQuery("SELECT ?x WHERE { ?x a ?type . }", OWLClassSParent.class)
                  .setParameter("type", URI.create(
                          Vocabulary.C_OWL_CLASS_S_PARENT)).getResultList();
        assertEquals(data.size(), result.size());

        boolean found;
        for (OWLClassT t : data) {
            found = false;
            for (OWLClassSParent tt : result) {
                if (t.getUri().equals(tt.getUri())) {
                    found = true;
                    assertTrue(tt instanceof OWLClassT);
                    verifyEntityTAttributes(t, (OWLClassT) tt);
                    break;
                }
            }
            assertTrue(found);
        }
    }

    @Test
    void selectLoadsInstanceAsGivenTypeWhenItIsConcreteAndFoundInTypesOfIndividual() {
        final OWLClassT t = Generators.getRandomItem(QueryTestEnvironment.getData(OWLClassT.class));
        final EntityManager em = getEntityManager();
        em.getTransaction().begin();
        t.setTypes(Collections.singleton(Vocabulary.C_OWL_CLASS_S));
        em.merge(t);
        em.getTransaction().commit();

        final OWLClassS result = em.createNativeQuery("SELECT ?x WHERE { ?x ?hasInt ?int. }", OWLClassS.class)
                                   .setParameter("hasInt", URI.create(Vocabulary.P_T_INTEGER_ATTRIBUTE))
                                   .setParameter("int", t.getIntAttribute()).getSingleResult();
        assertNotNull(result);
        assertFalse(result instanceof OWLClassT);
        assertEquals(t.getName(), result.getName());
        assertEquals(t.getDescription(), result.getDescription());
        assertTrue(result.getTypes().contains(Vocabulary.C_OWL_CLASS_T));
    }
}

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
package cz.cvut.kbss.jopa.test.integration.owlapi;

import cz.cvut.kbss.jopa.proxy.lazy.LazyLoadingProxy;
import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.OWLClassI;
import cz.cvut.kbss.jopa.test.Vocabulary;
import cz.cvut.kbss.jopa.test.environment.OwlapiDataAccessor;
import cz.cvut.kbss.jopa.test.environment.OwlapiPersistenceFactory;
import cz.cvut.kbss.jopa.test.runner.DeleteOperationsRunner;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNull;

public class DeleteOperationsTest extends DeleteOperationsRunner {

    private static final Logger LOG = LoggerFactory.getLogger(DeleteOperationsTest.class);

    public DeleteOperationsTest() {
        super(LOG, new OwlapiPersistenceFactory(), new OwlapiDataAccessor());
    }

    @Disabled
    @Test
    @Override
    public void settingDatatypeCollectionToNullRemovesAllValues() {
        // Another issue with OWL2Query, causes reasoner exception
    }

    @Disabled
    @Test
    @Override
    public void clearingDatatypeCollectionRemovesAllValues() {
        // Another issue with OWL2Query, causes reasoner exception
    }

    @Test
    @Override
    public void removeTriggersLazyLoading() {
        // OWL doesn't remove all statements, unlike RDF4J and Jena, but keeps certain top-level attributes. That's why this test has to be overriden
        this.em = getEntityManager("removeTriggersLazyLoading", true);
        persist(entityI);

        em.getEntityManagerFactory().getCache().evictAll();
        final OWLClassI resI = findRequired(OWLClassI.class, entityI.getUri());
        assertInstanceOf(LazyLoadingProxy.class, resI.getOwlClassA());

        em.getTransaction().begin();
        em.remove(resI.getOwlClassA());
        em.getTransaction().commit();

        assertNull(em.find(OWLClassA.class, entityA.getUri()));

        // verify that the attributes of A were removed
        assertFalse(
                em.createNativeQuery("ASK { ?x <"+ Vocabulary.P_A_STRING_ATTRIBUTE + "> ?z .}", Boolean.class).setParameter("x", entityA.getUri())
                  .getSingleResult());
    }
}

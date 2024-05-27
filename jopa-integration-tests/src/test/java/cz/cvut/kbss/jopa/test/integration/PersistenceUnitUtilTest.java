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
package cz.cvut.kbss.jopa.test.integration;

import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.OWLClassL;
import cz.cvut.kbss.jopa.test.Vocabulary;
import cz.cvut.kbss.jopa.test.environment.Generators;
import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.AxiomImpl;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.net.URI;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class PersistenceUnitUtilTest extends IntegrationTestBase {

    @Test
    void isLoadedReturnsTrueForNewlyRegisteredEntity() {
        final OWLClassA entityA = new OWLClassA(Generators.generateUri());
        em.persist(entityA);
        assertTrue(em.getEntityManagerFactory().getPersistenceUnitUtil().isLoaded(entityA));
        assertTrue(em.getEntityManagerFactory().getPersistenceUnitUtil().isLoaded(entityA, "stringAttribute"));
        assertTrue(em.getEntityManagerFactory().getPersistenceUnitUtil().isLoaded(entityA, "types"));
    }

    @Test
    void isLoadedReturnsTrueForLoadedExistingEntity() throws Exception {
        final URI uri = Generators.generateUri();
        initAxiomsForOWLClassA(NamedResource.create(uri), "test", false);
        final OWLClassA entity = em.find(OWLClassA.class, uri);
        assertTrue(em.getEntityManagerFactory().getPersistenceUnitUtil().isLoaded(entity));
        assertTrue(em.getEntityManagerFactory().getPersistenceUnitUtil().isLoaded(entity, "stringAttribute"));
    }

    @Test
    void isLoadedReturnsFalseForUnloadedLazilyLoadedNonEmptyAttribute() throws Exception {
        final URI uri = Generators.generateUri();
        initOwlClassLAxioms(uri);
        final OWLClassL entity = em.find(OWLClassL.class, uri);
        assertNotNull(entity);
        assertFalse(em.getEntityManagerFactory().getPersistenceUnitUtil().isLoaded(entity, "singleA"));
        assertFalse(em.getEntityManagerFactory().getPersistenceUnitUtil().isLoaded(entity, "set"));
        assertFalse(em.getEntityManagerFactory().getPersistenceUnitUtil().isLoaded(entity, "referencedList"));
        assertFalse(em.getEntityManagerFactory().getPersistenceUnitUtil().isLoaded(entity, "simpleList"));
    }

    private void initOwlClassLAxioms(URI uri) throws Exception {
        final NamedResource subject = NamedResource.create(uri);
        final Assertion simpleList = Assertion.createObjectPropertyAssertion(URI.create(Vocabulary.p_l_simpleListAttribute), false);
        final Assertion referencedList = Assertion.createObjectPropertyAssertion(URI.create(Vocabulary.p_l_referencedListAttribute), false);
        final Assertion aSet = Assertion.createObjectPropertyAssertion(URI.create(Vocabulary.p_l_aSetAttribute), false);
        final Assertion singleA = Assertion.createObjectPropertyAssertion(URI.create(Vocabulary.p_l_singleOwlClassAAttribute), false);
        final Axiom<?> classAssertion = new AxiomImpl<>(subject, Assertion.createClassAssertion(false),
                new Value<>(NamedResource.create(Vocabulary.C_OWL_CLASS_L)));
        final List<Axiom<?>> axioms = List.of(
                classAssertion,
                new AxiomImpl<>(subject, simpleList, new Value<>(NamedResource.create(Generators.generateUri()))),
                new AxiomImpl<>(subject, referencedList, new Value<>(NamedResource.create(Generators.generateUri()))),
                new AxiomImpl<>(subject, aSet, new Value<>(NamedResource.create(Generators.generateUri()))),
                new AxiomImpl<>(subject, singleA, new Value<>(NamedResource.create(Generators.generateUri())))
        );
        final AxiomDescriptor desc = new AxiomDescriptor(subject);
        desc.addAssertion(Assertion.createClassAssertion(false));
        desc.addAssertion(simpleList);
        desc.addAssertion(referencedList);
        desc.addAssertion(aSet);
        desc.addAssertion(singleA);
        when(connectionMock.find(desc)).thenReturn(axioms);
        when(connectionMock.contains(classAssertion, null)).thenReturn(true);
    }

    @Test
    void isLoadedReturnsTrueForEntityWhenAllAttributesAreUnloadedLazyAttributes() throws Exception {
        final URI uri = Generators.generateUri();
        initOwlClassLAxioms(uri);
        final OWLClassL entity = em.find(OWLClassL.class, uri);
        assertNotNull(entity);
        assertTrue(em.getEntityManagerFactory().getPersistenceUnitUtil().isLoaded(entity));
    }
}

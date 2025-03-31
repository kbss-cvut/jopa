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
package cz.cvut.kbss.jopa.test.integration;

import cz.cvut.kbss.jopa.model.JOPAPersistenceProperties;
import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.OWLClassF;
import cz.cvut.kbss.jopa.test.Vocabulary;
import cz.cvut.kbss.jopa.test.environment.Generators;
import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.AxiomValueDescriptor;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.AxiomImpl;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.junit.jupiter.MockitoExtension;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
public class EntityManagerTest extends IntegrationTestBase {

    @Test
    void mergeWithNullInferredValueIgnoresChangeWhenIgnoreInferredValueRemovalsOnMergeIsConfigured() throws Exception {
        this.em = emf.createEntityManager(Map.of(JOPAPersistenceProperties.IGNORE_INFERRED_VALUE_REMOVAL_ON_MERGE, Boolean.toString(true)));
        final List<Axiom<?>> axioms = new ArrayList<>();
        final URI subject = Generators.generateUri();
        final NamedResource nsSubject = NamedResource.create(subject);
        final String inferredValue = "inferred value";
        final Axiom<?> classAssertion = new AxiomImpl<>(nsSubject, Assertion.createClassAssertion(false),
                new Value<>(NamedResource.create(Vocabulary.C_OWL_CLASS_F)));
        axioms.add(classAssertion);
        final Axiom<String> inferredAx = new AxiomImpl<>(nsSubject,
                Assertion.createDataPropertyAssertion(URI.create(Vocabulary.P_F_STRING_ATTRIBUTE), true),
                new Value<>(inferredValue));
        axioms.add(inferredAx);
        when(connectionMock.isInferred(inferredAx, Collections.emptySet())).thenReturn(true);
        final AxiomDescriptor desc = new AxiomDescriptor(nsSubject);
        desc.addAssertion(Assertion.createClassAssertion(false));
        final Assertion dpAssertion = Assertion.createDataPropertyAssertion(URI.create(Vocabulary.P_F_STRING_ATTRIBUTE), true);
        desc.addAssertion(dpAssertion);
        desc.addAssertion(Assertion.createObjectPropertyAssertion(URI.create(Vocabulary.P_F_HAS_SIMPLE_SET), false));
        doReturn(axioms).when(connectionMock).find(desc);
        doReturn(true).when(connectionMock).contains(classAssertion, Collections.emptySet());

        final OWLClassF toMerge = new OWLClassF(subject);
        assertNull(toMerge.getSecondStringAttribute());
        final OWLClassF result = em.merge(toMerge);
        verify(connectionMock, never()).update(any(AxiomValueDescriptor.class));
        assertEquals(inferredValue, result.getSecondStringAttribute());
    }

    @Test
    void flushWritesChangesToRepository() throws Exception {
        final OWLClassA entity = new OWLClassA(Generators.generateUri());
        entity.setStringAttribute("Test string");
        em.getTransaction().begin();
        em.persist(entity);
        em.flush();
        final ArgumentCaptor<AxiomValueDescriptor> captor = ArgumentCaptor.forClass(AxiomValueDescriptor.class);
        verify(connectionMock).persist(captor.capture());
        assertEquals(entity.getUri(), captor.getValue().getSubject().getIdentifier());
        em.getTransaction().commit();
    }
}

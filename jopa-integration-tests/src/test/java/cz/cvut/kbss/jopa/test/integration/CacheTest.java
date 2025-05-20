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

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.proxy.change.ChangeTrackingIndirectSet;
import cz.cvut.kbss.jopa.sessions.cache.CacheManager;
import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.OWLClassB;
import cz.cvut.kbss.jopa.test.OWLClassF;
import cz.cvut.kbss.jopa.test.Vocabulary;
import cz.cvut.kbss.jopa.test.environment.Generators;
import cz.cvut.kbss.ontodriver.Properties;
import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.Statement;
import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.iteration.ResultRow;
import cz.cvut.kbss.ontodriver.iteration.ResultSetIterator;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.AxiomImpl;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.Matchers.not;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class CacheTest extends IntegrationTestBase {

    @Mock
    private Statement statementMock;
    @Mock
    private ResultSet resultSetMock;
    @Mock
    private ResultRow resultRowMock;
    @Mock
    private ResultSetIterator resultSetIteratorMock;

    @BeforeEach
    public void setUp() throws Exception {
        super.setUp();
    }

    @Test
    void queryResultIsLoadedFromCacheWhenItIsAlreadyCached() throws Exception {
        when(connectionMock.createStatement()).thenReturn(statementMock);
        when(resultSetMock.iterator()).thenReturn(resultSetIteratorMock);
        when(resultSetIteratorMock.next()).thenReturn(resultRowMock);
        final URI instanceUri = Generators.generateUri();
        final String query = "SELECT ?x WHERE { ?x a <" + Vocabulary.C_OWL_CLASS_A + "> . }";
        when(statementMock.executeQuery(query)).thenReturn(resultSetMock);
        when(resultSetIteratorMock.hasNext()).thenReturn(true).thenReturn(false);
        when(resultRowMock.isBound(0)).thenReturn(true);
        when(resultRowMock.getString(0)).thenReturn(instanceUri.toString());
        when(connectionMock.find(any())).thenReturn(axiomsForA(instanceUri));
        final OWLClassA firstA = em.find(OWLClassA.class, instanceUri);
        assertNotNull(firstA);
        try (EntityManager emTwo = emf.createEntityManager()) {
            final OWLClassA secondA = emTwo.createNativeQuery(query, OWLClassA.class).getSingleResult();
            assertNotNull(secondA);
        }
        verify(connectionMock).find(any(AxiomDescriptor.class));
    }

    private Collection<Axiom<?>> axiomsForA(URI identifier) {
        final Collection<Axiom<?>> axioms = new ArrayList<>();
        final NamedResource nr = NamedResource.create(identifier);
        axioms.add(new AxiomImpl<>(nr, Assertion.createClassAssertion(false),
                                   new Value<>(NamedResource.create(Vocabulary.C_OWL_CLASS_A))));
        axioms.add(new AxiomImpl<>(nr,
                                   Assertion.createDataPropertyAssertion(URI.create(Vocabulary.P_A_STRING_ATTRIBUTE),
                                                                         false), new Value<>("stringAttribute")));
        return axioms;
    }

    @Test
    void loadedInstanceAddedToCacheDoesNotContainIndirectCollection() throws Exception {
        final URI id = Generators.generateUri();
        final Collection<Axiom<?>> axioms = axiomsForA(id);
        axioms.add(new AxiomImpl<>(NamedResource.create(id), Assertion.createClassAssertion(false),
                                   new Value<>(NamedResource.create(Vocabulary.C_OWL_CLASS_Q))));
        when(connectionMock.find(any())).thenReturn(axioms);
        final OWLClassA a = em.find(OWLClassA.class, id);
        assertNotNull(a);
        final CacheManager cacheManager = (CacheManager) em.getEntityManagerFactory().getCache();
        final OWLClassA result = cacheManager.get(OWLClassA.class, id, new EntityDescriptor());
        assertNotNull(result);
        assertNotNull(result.getTypes());
        assertThat(result.getTypes(), not(instanceOf(ChangeTrackingIndirectSet.class)));
    }

    @Test
    void transactionCommitEvictsClassesWithInferenceFromCache() throws Exception {
        final URI uri = Generators.generateUri();
        final Collection<Axiom<?>> axioms = Collections.singleton(
                new AxiomImpl<>(NamedResource.create(uri), Assertion.createClassAssertion(false),
                                new Value<>(NamedResource.create(Vocabulary.C_OWL_CLASS_F))));
        when(connectionMock.find(any())).thenReturn(axioms);
        final Descriptor descriptor = em.createDescriptor(OWLClassF.class);
        em.getTransaction().begin();
        final OWLClassF entityF = em.find(OWLClassF.class, uri);
        assertTrue(emf.getCache().contains(OWLClassF.class, entityF.getUri(), descriptor));
        final OWLClassA newA = new OWLClassA(Generators.generateUri());
        entityF.setSimpleSet(Collections.singleton(newA));
        em.persist(newA);
        em.getTransaction().commit();
        assertFalse(emf.getCache().contains(OWLClassF.class, entityF.getUri(), descriptor));
    }

    @Test
    void mergeIntoCacheResetsIndirectCollectionsToWrappedInstances() throws Exception {
        final Properties properties = mock(Properties.class);
        when(connectionMock.properties()).thenReturn(properties);
        final OWLClassB b = new OWLClassB();
        b.setUri(Generators.generateUri());
        b.setProperties(Collections.singletonMap(Vocabulary.P_Q_STRING_ATTRIBUTE, Collections.singleton("Test")));
        em.getTransaction().begin();
        em.persist(b);
        em.getTransaction().commit();
        Mockito.reset(connectionMock);

        final OWLClassB result = em.find(OWLClassB.class, b.getUri());
        assertEquals(b.getProperties(), result.getProperties());
        verify(connectionMock, never()).update(any());
    }
}

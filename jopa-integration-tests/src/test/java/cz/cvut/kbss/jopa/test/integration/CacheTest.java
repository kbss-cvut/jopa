/**
 * Copyright (C) 2016 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.test.integration;

import cz.cvut.kbss.jopa.adapters.IndirectSet;
import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.sessions.CacheManager;
import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.Vocabulary;
import cz.cvut.kbss.jopa.test.environment.Generators;
import cz.cvut.kbss.jopa.test.integration.environment.TestDataSource;
import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.Statement;
import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.model.*;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class CacheTest extends IntegrationTestBase {

    @Mock
    private Connection connectionMock;
    @Mock
    private Statement statementMock;
    @Mock
    private ResultSet resultSetMock;

    @Before
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        super.setUp();
        final TestDataSource ds = getDataSource();
        ds.setConnection(connectionMock);
        when(connectionMock.createStatement()).thenReturn(statementMock);
    }

    @Test
    public void queryResultIsLoadedFromCacheWhenItIsAlreadyCached() throws Exception {
        final URI instanceUri = Generators.generateUri();
        final String query = "SELECT ?x WHERE { ?x a <" + Vocabulary.C_OWL_CLASS_A + "> . }";
        when(statementMock.executeQuery(query)).thenReturn(resultSetMock);
        when(resultSetMock.hasNext()).thenReturn(true).thenReturn(false);
        when(resultSetMock.isBound(0)).thenReturn(true);
        when(resultSetMock.getString(0)).thenReturn(instanceUri.toString());
        when(connectionMock.find(any(AxiomDescriptor.class))).thenReturn(axiomsForA(instanceUri));
        final OWLClassA firstA = em.find(OWLClassA.class, instanceUri);
        assertNotNull(firstA);
        final EntityManager emTwo = emf.createEntityManager();
        try {
            final OWLClassA secondA = emTwo.createNativeQuery(query, OWLClassA.class).getSingleResult();
            assertNotNull(secondA);
        } finally {
            emTwo.close();
        }
        verify(connectionMock).find(any(AxiomDescriptor.class));
    }

    private Collection<Axiom<?>> axiomsForA(URI identifier) {
        final Collection<Axiom<?>> axioms = new ArrayList<>();
        final NamedResource nr = NamedResource.create(identifier);
        axioms.add(new AxiomImpl<>(nr, Assertion.createClassAssertion(false),
                new Value<>(NamedResource.create(Vocabulary.C_OWL_CLASS_A))));
        axioms.add(new AxiomImpl<>(nr,
                Assertion.createDataPropertyAssertion(URI.create(Vocabulary.P_A_STRING_ATTRIBUTE), false),
                new Value<>("stringAttribute")));
        return axioms;
    }

    @Test
    public void loadedInstanceAddedToCacheDoesNotContainIndirectCollection() throws Exception {
        final URI id = Generators.generateUri();
        final Collection<Axiom<?>> axioms = axiomsForA(id);
        axioms.add(new AxiomImpl<>(NamedResource.create(id), Assertion.createClassAssertion(false),
                new Value<>(NamedResource.create(Vocabulary.C_OWL_CLASS_Q))));
        when(connectionMock.find(any(AxiomDescriptor.class))).thenReturn(axioms);
        final OWLClassA a = em.find(OWLClassA.class, id);
        assertNotNull(a);
        final CacheManager cacheManager = (CacheManager) em.getEntityManagerFactory().getCache();
        final OWLClassA result = cacheManager.get(OWLClassA.class, id, new EntityDescriptor());
        assertNotNull(result);
        assertNotNull(result.getTypes());
        assertFalse(result.getTypes() instanceof IndirectSet);
    }
}

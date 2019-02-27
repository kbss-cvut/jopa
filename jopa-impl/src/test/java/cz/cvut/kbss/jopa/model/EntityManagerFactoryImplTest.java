/**
 * Copyright (C) 2019 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.model;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.utils.DataSourceStub;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.Types;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.HashMap;
import java.util.Map;

import static org.junit.Assert.*;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class EntityManagerFactoryImplTest {

    private EntityManagerFactoryImpl emf;

    @Mock
    private Connection connection;

    @Before
    public void setUp() {
        MockitoAnnotations.initMocks(this);
        final Map<String, String> props = new HashMap<>();
        props.put(JOPAPersistenceProperties.DATA_SOURCE_CLASS, DataSourceStub.class.getName());
        props.put(JOPAPersistenceProperties.ONTOLOGY_PHYSICAL_URI_KEY,
                Generators.createIndividualIdentifier().toString());
        props.put(JOPAPersistenceProperties.SCAN_PACKAGE, "cz.cvut.kbss.jopa.environment");
        this.emf = new EntityManagerFactoryImpl(props);
        emf.createEntityManager();
        emf.getServerSession().unwrap(DataSourceStub.class).setConnection(connection);
        when(connection.types()).thenReturn(mock(Types.class));
    }

    @Test
    public void isLoadedReturnsTrueForManagedInstance() {
        final EntityManager em = emf.createEntityManager();
        try {
            final OWLClassA a = Generators.generateOwlClassAInstance();
            em.persist(a);
            assertTrue(emf.isLoaded(a));
        } finally {
            em.close();
        }
    }

    @Test
    public void isLoadedReturnsTrueForAttributeOfManagedInstance() throws Exception {
        final EntityManager em = emf.createEntityManager();
        try {
            final OWLClassA a = Generators.generateOwlClassAInstance();
            em.persist(a);
            assertTrue(emf.isLoaded(a, OWLClassA.getStrAttField().getName()));
        } finally {
            em.close();
        }
    }

    @Test
    public void isLoadedReturnsFalseNonNonManagedInstance() {
        final EntityManager emOne = emf.createEntityManager();
        final EntityManager emTwo = emf.createEntityManager();
        try {
            assertFalse(emf.isLoaded(Generators.generateOwlClassAInstance()));
        } finally {
            emOne.close();
            emTwo.close();
        }
    }

    @Test
    public void isLoadedReturnsFalseNonNonManagedInstanceWithAttribute() throws Exception {
        final EntityManager emOne = emf.createEntityManager();
        final EntityManager emTwo = emf.createEntityManager();
        try {
            assertFalse(emf.isLoaded(Generators.generateOwlClassAInstance(), OWLClassA.getStrAttField().getName()));
        } finally {
            emOne.close();
            emTwo.close();
        }
    }

    @Test
    public void unwrapReturnsReturnsEntityManagerFactoryImplWhenTypeMatches() {
        final EntityManagerFactoryImpl result = emf.unwrap(EntityManagerFactoryImpl.class);
        assertNotNull(result);
        assertSame(emf, result);
    }

    @Test
    public void unwrapForwardsCallToServerSessionWhenClassDoesNotMatch() {
        final DataSourceStub result = emf.unwrap(DataSourceStub.class);
        assertSame(emf.getServerSession().unwrap(DataSourceStub.class), result);
    }
}
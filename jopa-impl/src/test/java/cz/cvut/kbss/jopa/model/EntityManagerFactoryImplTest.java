/**
 * Copyright (C) 2020 Czech Technical University in Prague
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
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.HashMap;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class EntityManagerFactoryImplTest {

    private EntityManagerFactoryImpl emf;

    @Mock
    private Connection connection;

    @BeforeEach
    void setUp() {
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
    void isLoadedReturnsTrueForManagedInstance() {
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
    void isLoadedReturnsTrueForAttributeOfManagedInstance() throws Exception {
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
    void isLoadedReturnsFalseNonNonManagedInstance() {
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
    void isLoadedReturnsFalseNonNonManagedInstanceWithAttribute() throws Exception {
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
    void unwrapReturnsReturnsEntityManagerFactoryImplWhenTypeMatches() {
        final EntityManagerFactoryImpl result = emf.unwrap(EntityManagerFactoryImpl.class);
        assertNotNull(result);
        assertSame(emf, result);
    }

    @Test
    void unwrapForwardsCallToServerSessionWhenClassDoesNotMatch() {
        final DataSourceStub result = emf.unwrap(DataSourceStub.class);
        assertSame(emf.getServerSession().unwrap(DataSourceStub.class), result);
    }

    @Test
    void closeClosesAllOpenEntityManagers() {
        final EntityManager emOne = emf.createEntityManager();
        final EntityManager emTwo = emf.createEntityManager();
        assertTrue(emOne.isOpen());
        assertTrue(emTwo.isOpen());
        emf.close();
        assertFalse(emf.isOpen());
        assertFalse(emOne.isOpen());
        assertFalse(emTwo.isOpen());
    }

    @Test
    void createEntityManagerOnClosedInstanceThrowsIllegalStateException() {
        emf.close();
        assertThrows(IllegalStateException.class, () -> emf.createEntityManager());
    }

    @Test
    void getIdentifiersExtractsEntityIdentifier() {
        final OWLClassA instance = Generators.generateOwlClassAInstance();
        assertEquals(instance.getUri(), emf.getIdentifier(instance));
    }
}

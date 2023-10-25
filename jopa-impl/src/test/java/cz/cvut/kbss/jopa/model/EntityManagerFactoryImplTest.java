/*
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.model;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.utils.DataSourceStub;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.sessions.CacheManager;
import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.Types;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.HashMap;
import java.util.Map;
import java.util.function.Consumer;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class EntityManagerFactoryImplTest {

    private EntityManagerFactoryImpl emf;

    @Mock
    private Connection connection;

    @Mock
    Consumer<EntityManagerFactoryImpl> closeListener;

    @BeforeEach
    void setUp() {
        final Map<String, String> props = new HashMap<>();
        props.put(JOPAPersistenceProperties.DATA_SOURCE_CLASS, DataSourceStub.class.getName());
        props.put(JOPAPersistenceProperties.ONTOLOGY_PHYSICAL_URI_KEY,
                Generators.createIndividualIdentifier().toString());
        props.put(JOPAPersistenceProperties.SCAN_PACKAGE, "cz.cvut.kbss.jopa.environment");
        this.emf = new EntityManagerFactoryImpl(props, closeListener);
        emf.createEntityManager();
        emf.getServerSession().unwrap(DataSourceStub.class).setConnection(connection);
    }

    @Test
    void isLoadedReturnsTrueForManagedInstance() {
        final EntityManager em = emf.createEntityManager();
        final OWLClassA a = Generators.generateOwlClassAInstance();
        em.persist(a);
        assertTrue(emf.isLoaded(a));
    }

    @Test
    void isLoadedReturnsTrueForAttributeOfManagedInstance() throws Exception {
        final EntityManager em = emf.createEntityManager();
        final OWLClassA a = Generators.generateOwlClassAInstance();
        em.persist(a);
        assertTrue(emf.isLoaded(a, OWLClassA.getStrAttField().getName()));
    }

    @Test
    void isLoadedReturnsFalseNonNonManagedInstance() {
        emf.createEntityManager();
        emf.createEntityManager();
        assertFalse(emf.isLoaded(Generators.generateOwlClassAInstance()));
    }

    @Test
    void isLoadedReturnsFalseNonNonManagedInstanceWithAttribute() throws Exception {
        emf.createEntityManager();
        emf.createEntityManager();
        assertFalse(emf.isLoaded(Generators.generateOwlClassAInstance(), OWLClassA.getStrAttField().getName()));
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

    @Test
    void closeClearsSecondLevelCache() {
        final OWLClassA instance = Generators.generateOwlClassAInstance();
        final CacheManager cache = emf.unwrap(CacheManager.class);
        cache.add(instance.getUri(), instance, new EntityDescriptor());
        assertTrue(cache.contains(OWLClassA.class, instance.getUri(), new EntityDescriptor()));
        emf.close();
        assertFalse(cache.contains(OWLClassA.class, instance.getUri(), new EntityDescriptor()));
    }

    @Test
    void closeInvokesCloseListener() {
        emf.close();
        verify(closeListener).accept(emf);
    }
}

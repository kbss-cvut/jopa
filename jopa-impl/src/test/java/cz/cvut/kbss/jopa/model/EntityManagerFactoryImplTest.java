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
package cz.cvut.kbss.jopa.model;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.utils.DataSourceStub;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.sessions.ReadOnlyUnitOfWork;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;
import cz.cvut.kbss.jopa.sessions.cache.CacheManager;
import cz.cvut.kbss.jopa.sessions.cache.Descriptors;
import cz.cvut.kbss.jopa.sessions.descriptor.LoadStateDescriptor;
import cz.cvut.kbss.ontodriver.Connection;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Map;
import java.util.function.Consumer;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

@ExtendWith(MockitoExtension.class)
class EntityManagerFactoryImplTest {

    private EntityManagerFactoryImpl emf;

    @Mock
    private Connection connection;

    @Mock
    Consumer<EntityManagerFactoryImpl> closeListener;

    @BeforeEach
    void setUp() {
        this.emf = new EntityManagerFactoryImpl(getProps(), closeListener);
        emf.createEntityManager();
        emf.getServerSession().unwrap(DataSourceStub.class).setConnection(connection);
    }

    private Map<String, String> getProps() {
        return Map.of(JOPAPersistenceProperties.DATA_SOURCE_CLASS, DataSourceStub.class.getName(),
                JOPAPersistenceProperties.ONTOLOGY_PHYSICAL_URI_KEY,
                Generators.createIndividualIdentifier().toString(),
                JOPAPersistenceProperties.SCAN_PACKAGE, "cz.cvut.kbss.jopa.environment");
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
        cache.add(instance.getUri(), instance, new Descriptors(new EntityDescriptor(), new LoadStateDescriptor<>(instance, mock(EntityType.class), LoadState.LOADED)));
        assertTrue(cache.contains(OWLClassA.class, instance.getUri(), new EntityDescriptor()));
        emf.close();
        assertFalse(cache.contains(OWLClassA.class, instance.getUri(), new EntityDescriptor()));
    }

    @Test
    void closeInvokesCloseListener() {
        emf.close();
        verify(closeListener).accept(emf);
    }

    @Test
    void entityManagerFactoryIsAutoCloseable() {
        try (final EntityManagerFactory emf = new EntityManagerFactoryImpl(getProps(), closeListener)) {
            assertTrue(emf.isOpen());
        }
        verify(closeListener).accept(any(EntityManagerFactoryImpl.class));
    }

    @Test
    void createEntityManagerWithReadOnlyTransactionModeReturnsEntityManagerWithReadOnlyPersistenceContext() {
        final EntityManager em = emf.createEntityManager(Map.of(JOPAPersistenceProperties.TRANSACTION_MODE, "read_only"));
        assertInstanceOf(ReadOnlyUnitOfWork.class, em.unwrap(UnitOfWork.class));
    }
}

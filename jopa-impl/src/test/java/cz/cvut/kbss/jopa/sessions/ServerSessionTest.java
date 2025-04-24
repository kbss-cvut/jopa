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
package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.accessors.DataSourceStub;
import cz.cvut.kbss.jopa.accessors.StorageAccessor;
import cz.cvut.kbss.jopa.model.AbstractEntityManager;
import cz.cvut.kbss.jopa.model.JOPAPersistenceProperties;
import cz.cvut.kbss.jopa.model.MetamodelImpl;
import cz.cvut.kbss.jopa.transactions.EntityTransaction;
import cz.cvut.kbss.jopa.utils.Configuration;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;
import org.junit.jupiter.params.provider.Arguments;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.net.URI;
import java.util.Collections;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.Mockito.*;

public class ServerSessionTest {

    @Mock
    private MetamodelImpl metamodelMock;

    private ServerSession session;

    @BeforeEach
    public void setUp() {
        MockitoAnnotations.openMocks(this);
        OntologyStorageProperties storageProperties = OntologyStorageProperties.ontologyUri(
                URI.create("http://krizik.felk.cvut.cz/ontologies/jopa")).physicalUri(
                URI.create("file://tmp/jopa")).driver(DataSourceStub.class.getCanonicalName()).build();
        when(metamodelMock.getEntities()).thenReturn(Collections.emptySet());
        this.session = new ServerSession(storageProperties, new Configuration(Collections.emptyMap()), metamodelMock);
    }

    @Test
    public void testClose() {
        final EntityTransaction et = mock(EntityTransaction.class);
        when(et.isActive()).thenReturn(Boolean.TRUE);
        when(et.isRollbackOnly()).thenReturn(Boolean.FALSE);
        final AbstractEntityManager em = mock(AbstractEntityManager.class);
        session.transactionStarted(et, em);

        session.close();
        verify(et).setRollbackOnly();
    }

    @Test
    public void unwrapServerSessionReturnsItself() {
        assertSame(session, session.unwrap(ServerSession.class));
    }

    @Test
    public void unwrapCallThroughToStorageAccessorIfClassDoesNotMatch() {
        final StorageAccessor sa = session.unwrap(StorageAccessor.class);
        assertNotNull(sa);
    }

    static Stream<Arguments> unitOfWorkModeProvider() {
        return Stream.of(
                Arguments.of(JOPAPersistenceProperties.TRANSACTION_MODE, "read_only", ReadOnlyUnitOfWork.class),
                Arguments.of(JOPAPersistenceProperties.CHANGE_TRACKING_MODE, "immediate", ChangeTrackingUnitOfWork.class),
                Arguments.of(JOPAPersistenceProperties.CHANGE_TRACKING_MODE, "on_commit", OnCommitChangePropagatingUnitOfWork.class),
                Arguments.of(JOPAPersistenceProperties.CHANGE_TRACKING_MODE, "invalid_mode", ChangeTrackingUnitOfWork.class),
                Arguments.of("", "", ChangeTrackingUnitOfWork.class)
        );
    }

    @ParameterizedTest
    @MethodSource("unitOfWorkModeProvider")
    void acquireUnitOfWorkReturnsCorrectType(String property, String transactionMode,
                                             Class<AbstractUnitOfWork> expectedUOWClass) {
        Configuration config = new Configuration();
        config.set(property, transactionMode);

        UnitOfWork uow = session.acquireUnitOfWork(config);

        assertInstanceOf(expectedUOWClass, uow);
    }
}

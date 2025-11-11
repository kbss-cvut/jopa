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
package cz.cvut.kbss.ontodriver.rdf4j.connector;

import cz.cvut.kbss.ontodriver.rdf4j.environment.Generator;
import cz.cvut.kbss.ontodriver.rdf4j.exception.Rdf4jDriverException;
import org.eclipse.rdf4j.common.transaction.IsolationLevel;
import org.eclipse.rdf4j.common.transaction.IsolationLevels;
import org.eclipse.rdf4j.model.IRI;
import org.eclipse.rdf4j.model.ValueFactory;
import org.eclipse.rdf4j.model.impl.SimpleValueFactory;
import org.eclipse.rdf4j.model.vocabulary.RDF;
import org.eclipse.rdf4j.model.vocabulary.RDFS;
import org.eclipse.rdf4j.repository.Repository;
import org.eclipse.rdf4j.repository.RepositoryConnection;
import org.eclipse.rdf4j.repository.RepositoryException;
import org.eclipse.rdf4j.repository.sail.SailRepository;
import org.eclipse.rdf4j.sail.inferencer.fc.SchemaCachingRDFSInferencer;
import org.eclipse.rdf4j.sail.memory.MemoryStore;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import java.net.URI;
import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

class StorageConnectionTest {

    private Repository repository;

    private StorageConnection sut;

    @AfterEach
    void tearDown() throws Exception {
        if (repository != null && repository.isInitialized()) {
            if (sut != null) {
                sut.close();
            }
            repository.shutDown();
        }
    }

    @Test
    void isInferredReturnsTrueWhenStatementIsInferredInSpecifiedContext() throws Exception {
        this.repository = new SailRepository(new SchemaCachingRDFSInferencer(new MemoryStore()));
        final StorageConnector connector = mock(StorageConnector.class);
        when(connector.acquireConnection()).thenReturn(repository.getConnection());
        this.sut = new StorageConnection(connector, null);

        final ValueFactory vf = SimpleValueFactory.getInstance();
        final IRI childType = vf.createIRI(Generator.generateUri().toString());
        final IRI parentType = vf.createIRI(Generator.generateUri().toString());
        final IRI instance = vf.createIRI(Generator.generateUri().toString());
        final URI context = Generator.generateUri();
        try (final RepositoryConnection conn = repository.getConnection()) {
            conn.begin();
            conn.add(childType, RDFS.SUBCLASSOF, parentType, vf.createIRI(context.toString()));
            conn.add(instance, RDF.TYPE, childType, vf.createIRI(context.toString()));
            conn.commit();
        }

        sut.begin();
        try {
            assertFalse(sut.isInferred(vf.createStatement(instance, RDF.TYPE, parentType), Collections.singleton(vf.createIRI(Generator.generateUri()
                                                                                                                                       .toString()))));
            assertTrue(sut.isInferred(vf.createStatement(instance, RDF.TYPE, parentType), Collections.singleton(vf.createIRI(context.toString()))));
            assertTrue(sut.isInferred(vf.createStatement(instance, RDF.TYPE, parentType), Collections.emptySet()));
        } finally {
            sut.rollback();
        }
    }

    @Test
    void beginUsesConfiguredTransactionIsolationLevel() throws Exception {
        this.repository = new SailRepository(new MemoryStore());
        final ValueFactory vf = repository.getValueFactory();
        final IRI subject = repository.getValueFactory().createIRI(Generator.generateUri().toString());
        try (final RepositoryConnection conn = repository.getConnection()) {
            conn.add(subject, RDFS.LABEL, vf.createLiteral("oldValue"));
        }
        final StorageConnector connector = mock(StorageConnector.class);
        doAnswer(inv -> repository.getConnection()).when(connector).acquireConnection();
        this.sut = new StorageConnection(connector, IsolationLevels.SERIALIZABLE);
        final StorageConnection sut2 = new StorageConnection(connector, IsolationLevels.SERIALIZABLE);

        sut.begin();
        sut2.begin();
        sut.removeStatements(sut.findStatements(subject, RDFS.LABEL, null, false));
        sut.addStatements(List.of(vf.createStatement(subject, RDFS.LABEL, vf.createLiteral("newValue1"))));
        sut2.removeStatements(sut2.findStatements(subject, RDFS.LABEL, null, false));
        sut2.addStatements(List.of(vf.createStatement(subject, RDFS.LABEL, repository.getValueFactory()
                                                                                     .createLiteral("newValue2"))));
        sut.commit();
        final Rdf4jDriverException ex = assertThrows(Rdf4jDriverException.class, sut2::commit);
        sut2.close();
        assertInstanceOf(RepositoryException.class, ex.getCause());
    }

    @Test
    void setReadOnlyPutsConnectionIntoReadOnlyMode() throws Exception {
        final StorageConnector connector = mock(StorageConnector.class);
        final RepositoryConnection conn = mock(RepositoryConnection.class);
        when(connector.acquireConnection()).thenReturn(conn);
        this.sut = new StorageConnection(connector, IsolationLevels.READ_UNCOMMITTED);
        sut.setReadOnly(true);
        assertTrue(sut.isReadOnly());
    }

    @Test
    void setReadOnlyThrowsIllegalStateExceptionWhenTransactionIsAlreadyActive() throws Exception {
        final StorageConnector connector = mock(StorageConnector.class);
        final RepositoryConnection conn = mock(RepositoryConnection.class);
        when(connector.acquireConnection()).thenReturn(conn);
        when(conn.isActive()).thenReturn(true);
        this.sut = new StorageConnection(connector, IsolationLevels.READ_UNCOMMITTED);
        try {
            sut.begin();
            assertThrows(IllegalStateException.class, () -> sut.setReadOnly(true));
        } finally {
            sut.rollback();
        }
    }

    @ParameterizedTest
    @CsvSource({"READ_UNCOMMITTED", "READ_COMMITTED", "SNAPSHOT_READ", "SNAPSHOT"})
    void beginDoesNotStartTransactionWhenIsolationLevelIsNotSerializable(String level) throws Exception {
        final StorageConnector connector = mock(StorageConnector.class);
        final RepositoryConnection conn = mock(RepositoryConnection.class);
        when(connector.acquireConnection()).thenReturn(conn);
        final IsolationLevels isolationLevel = IsolationLevels.valueOf(level);
        this.sut = new StorageConnection(connector, isolationLevel);
        sut.setReadOnly(true);
        sut.begin();
        verify(connector).acquireConnection();
        verify(conn, never()).begin(any(IsolationLevel.class));
    }

    @Test
    void commitDoesNotCommitWhenConnectionIsReadOnly() throws Exception {
        final StorageConnector connector = mock(StorageConnector.class);
        final RepositoryConnection conn = mock(RepositoryConnection.class);
        when(connector.acquireConnection()).thenReturn(conn);
        this.sut = new StorageConnection(connector, IsolationLevels.READ_UNCOMMITTED);
        sut.setReadOnly(true);
        sut.begin();
        sut.commit();
        verify(conn, never()).commit();
    }
}

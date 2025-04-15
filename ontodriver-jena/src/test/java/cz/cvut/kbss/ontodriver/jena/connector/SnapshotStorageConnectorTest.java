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
package cz.cvut.kbss.ontodriver.jena.connector;

import cz.cvut.kbss.ontodriver.Statement.StatementOntology;
import cz.cvut.kbss.ontodriver.config.DriverConfiguration;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.jena.environment.Generator;
import cz.cvut.kbss.ontodriver.jena.exception.JenaDriverException;
import cz.cvut.kbss.ontodriver.jena.query.AbstractResultSet;
import org.apache.jena.query.Dataset;
import org.apache.jena.query.Query;
import org.apache.jena.query.QueryFactory;
import org.apache.jena.rdf.model.Statement;
import org.apache.jena.vocabulary.RDF;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Field;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import static cz.cvut.kbss.ontodriver.jena.connector.StorageTestUtil.*;
import static org.apache.jena.rdf.model.ResourceFactory.createResource;
import static org.apache.jena.rdf.model.ResourceFactory.createStatement;
import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

public class SnapshotStorageConnectorTest {

    private SharedStorageConnector centralConnector;

    private SnapshotStorageConnector connector;

    @BeforeEach
    public void setUp() {
        final DriverConfiguration configuration = StorageTestUtil.createConfiguration("test:uri");
        this.centralConnector = spy(new SharedStorageConnector(configuration));
        this.connector = new SnapshotStorageConnector(centralConnector);
    }

    @Test
    public void beginCopiesDatasetFromCentralConnector() throws JenaDriverException {
        centralConnector.begin();
        final Statement existing = createStatement(createResource(SUBJECT), RDF.type, createResource(TYPE_ONE));
        centralConnector.add(Collections.singletonList(existing), null);
        centralConnector.commit();

        connector.begin();
        final Dataset dataset = connector.getStorage().getDataset();
        assertTrue(dataset.getDefaultModel().contains(existing));
        assertNotSame(centralConnector.getStorage().getDataset(), dataset);
        centralConnector.begin();
        centralConnector.remove(Collections.singletonList(existing), null);
        centralConnector.commit();
        assertTrue(dataset.getDefaultModel().contains(existing));
    }

    @Test
    public void beginCopiesDatasetNamedGraphsFromCentralConnector() throws JenaDriverException {
        final String context = Generator.generateUri().toString();
        centralConnector.begin();
        final Statement existing = createStatement(createResource(SUBJECT), RDF.type, createResource(TYPE_ONE));
        centralConnector.add(Collections.singletonList(existing), context);
        centralConnector.commit();

        connector.begin();
        final Dataset dataset = connector.getStorage().getDataset();
        assertTrue(dataset.getNamedModel(context).contains(existing));
        assertNotSame(centralConnector.getStorage().getDataset(), dataset);
        centralConnector.begin();
        centralConnector.remove(Collections.singletonList(existing), context);
        centralConnector.commit();
        assertTrue(dataset.getNamedModel(context).contains(existing));
    }

    @Test
    public void beginThrowsIllegalStateWhenTransactionIsAlreadyActive() {
        connector.begin();
        final IllegalStateException ex = assertThrows(IllegalStateException.class, () -> connector.begin());
        assertThat(ex.getMessage(), containsString("Transaction is already active"));
    }

    @Test
    public void rollbackDiscardsTransactionalSnapshot() {
        connector.begin();
        assertNotNull(connector.getStorage());
        connector.rollback();
        assertNull(connector.getStorage());
    }

    @Test
    public void addStatementsInTransactionsAddsThemToTransactionalChanges() throws Exception {
        connector.begin();
        final Statement added = createStatement(createResource(SUBJECT), RDF.type, createResource(TYPE_ONE));
        connector.add(Collections.singletonList(added), null);
        assertEquals(LocalModel.Containment.ADDED,
                getTransactionalChanges()
                        .contains(added.getSubject(), added.getPredicate(), added.getObject(), Collections.emptySet()));
    }

    private LocalModel getTransactionalChanges() throws Exception {
        final Field changesField = SnapshotStorageConnector.class.getDeclaredField("transactionalChanges");
        changesField.setAccessible(true);
        return (LocalModel) changesField.get(connector);
    }

    @Test
    public void addStatementsAddsThemToSnapshot() {
        connector.begin();
        final Statement added = createStatement(createResource(SUBJECT), RDF.type, createResource(TYPE_ONE));
        assertFalse(dataset().getDefaultModel()
                             .contains(added.getSubject(), added.getPredicate(), added.getObject()));
        connector.add(Collections.singletonList(added), null);
        assertTrue(dataset().getDefaultModel()
                            .contains(added.getSubject(), added.getPredicate(), added.getObject()));
    }

    private Dataset dataset() {
        return connector.getStorage().getDataset();
    }

    @Test
    public void rollbackDiscardsTransactionalChanges() throws Exception {
        connector.begin();
        final Statement added = createStatement(createResource(SUBJECT), RDF.type, createResource(TYPE_ONE));
        connector.add(Collections.singletonList(added), null);
        assertEquals(LocalModel.Containment.ADDED,
                getTransactionalChanges()
                        .contains(added.getSubject(), added.getPredicate(), added.getObject(), Collections.emptySet()));
        connector.rollback();
        assertNull(getTransactionalChanges());
    }

    @Test
    public void addAddsStatementsToCorrectContext() throws Exception {
        connector.begin();
        final String context = Generator.generateUri().toString();
        final Statement added = createStatement(createResource(SUBJECT), RDF.type, createResource(TYPE_ONE));
        connector.add(Collections.singletonList(added), context);
        assertTrue(dataset().getNamedModel(context)
                            .contains(added.getSubject(), added.getPredicate(), added.getObject()));
        assertEquals(LocalModel.Containment.ADDED,
                getTransactionalChanges()
                        .contains(added.getSubject(), added.getPredicate(), added.getObject(),
                                Collections.singleton(context)));
    }

    @Test
    public void findQueriesTransactionalSnapshot() {
        connector.begin();
        final Statement added = createStatement(createResource(SUBJECT), RDF.type, createResource(TYPE_ONE));
        connector.add(Collections.singletonList(added), null);
        final Collection<Statement> result = connector
                .find(added.getSubject(), added.getPredicate(), null, Collections.emptySet());
        assertTrue(result.contains(added));
        verify(centralConnector, never()).find(any(), any(), any(), anyCollection());
    }

    @Test
    public void findQueriesTransactionalSnapshotContext() {
        connector.begin();
        final String context = Generator.generateUri().toString();
        final Statement added = createStatement(createResource(SUBJECT), RDF.type, createResource(TYPE_ONE));
        connector.add(Collections.singletonList(added), context);
        final Collection<Statement> result = connector
                .find(added.getSubject(), added.getPredicate(), null, Collections.singleton(context));
        assertTrue(result.contains(added));
        verify(centralConnector, never()).find(any(), any(), any(), anyCollection());
        verify(centralConnector, never()).find(any(), any(), any(), eq(Collections.singleton(context)));
    }

    @Test
    public void containsQueriesTransactionalSnapshot() {
        connector.begin();
        final Statement added = createStatement(createResource(SUBJECT), RDF.type, createResource(TYPE_ONE));
        connector.add(Collections.singletonList(added), null);
        assertTrue(connector.contains(added.getSubject(), added.getPredicate(), null, Collections.emptySet()));
        verify(centralConnector, never()).contains(any(), any(), any(), anySet());
    }

    @Test
    public void containsQueriesTransactionalSnapshotContext() {
        connector.begin();
        final String context = Generator.generateUri().toString();
        final Statement added = createStatement(createResource(SUBJECT), RDF.type, createResource(TYPE_ONE));
        connector.add(Collections.singletonList(added), context);
        assertTrue(connector.contains(added.getSubject(), added.getPredicate(), null, Collections.singleton(context)));
        verify(centralConnector, never()).contains(any(), any(), any(), anySet());
        verify(centralConnector, never()).contains(any(), any(), any(), eq(Collections.singleton(context)));
    }

    @Test
    public void getContextsListsContextInSnapshot() {
        connector.begin();
        final String context = Generator.generateUri().toString();
        final Statement added = createStatement(createResource(SUBJECT), RDF.type, createResource(TYPE_ONE));
        connector.add(Collections.singletonList(added), context);
        final List<String> result = connector.getContexts();
        assertEquals(1, result.size());
        assertTrue(result.contains(context));
        verify(centralConnector, never()).getContexts();
    }

    @Test
    public void removeRemovesStatementsFromSnapshot() throws JenaDriverException {
        centralConnector.begin();
        final Statement existing = createStatement(createResource(SUBJECT), RDF.type, createResource(TYPE_ONE));
        centralConnector.add(Collections.singletonList(existing), null);
        centralConnector.commit();
        connector.begin();
        assertTrue(connector.contains(existing.getSubject(), existing.getPredicate(), existing.getObject(),
                Collections.emptySet()));
        connector.remove(Collections.singletonList(existing), null);
        assertFalse(connector.contains(existing.getSubject(), existing.getPredicate(), existing.getObject(),
                Collections.emptySet()));
    }

    @Test
    public void removeRemovesStatementsFromSnapshotContext() throws JenaDriverException {
        centralConnector.begin();
        final String context = Generator.generateUri().toString();
        final Statement existing = createStatement(createResource(SUBJECT), RDF.type, createResource(TYPE_ONE));
        centralConnector.add(Collections.singletonList(existing), context);
        centralConnector.commit();
        connector.begin();
        assertTrue(connector.contains(existing.getSubject(), existing.getPredicate(), existing.getObject(),
                Collections.singleton(context)));
        connector.remove(Collections.singletonList(existing), context);
        assertFalse(connector.contains(existing.getSubject(), existing.getPredicate(), existing.getObject(),
                Collections.singleton(context)));
    }

    @Test
    public void removeRemovesStatementsFilteredBySubjectPredicateObjectFromSnapshot() throws JenaDriverException {
        centralConnector.begin();
        final Statement existing = createStatement(createResource(SUBJECT), RDF.type, createResource(TYPE_ONE));
        centralConnector.add(Collections.singletonList(existing), null);
        centralConnector.commit();
        connector.begin();
        assertTrue(connector.contains(existing.getSubject(), existing.getPredicate(), existing.getObject(),
                Collections.emptySet()));
        connector.remove(existing.getSubject(), existing.getPredicate(), null, null);
        assertFalse(connector.contains(existing.getSubject(), existing.getPredicate(), existing.getObject(),
                Collections.emptySet()));
    }

    @Test
    public void removeRemovesStatementsFilteredBySubjectPredicateObjectFromSnapshotContext() throws
            JenaDriverException {
        centralConnector.begin();
        final String context = Generator.generateUri().toString();
        final Statement existing = createStatement(createResource(SUBJECT), RDF.type, createResource(TYPE_ONE));
        centralConnector.add(Collections.singletonList(existing), context);
        centralConnector.commit();
        connector.begin();
        assertTrue(connector.contains(existing.getSubject(), existing.getPredicate(), existing.getObject(),
                Collections.singleton(context)));
        connector.remove(existing.getSubject(), null, existing.getObject(), context);
        assertFalse(connector.contains(existing.getSubject(), existing.getPredicate(), existing.getObject(),
                Collections.singleton(context)));
    }

    @Test
    public void commitAppliesChangesToCentralConnector() throws JenaDriverException {
        centralConnector.begin();
        final Statement existing = createStatement(createResource(SUBJECT), RDF.type, createResource(TYPE_ONE));
        centralConnector.add(Collections.singletonList(existing), null);
        centralConnector.commit();
        reset(centralConnector);

        connector.begin();
        connector.remove(existing.getSubject(), existing.getPredicate(), existing.getObject(), null);
        final Statement added = createStatement(createResource(SUBJECT), RDF.type, createResource(TYPE_TWO));
        connector.add(Collections.singletonList(added), null);
        connector.commit();
        verify(centralConnector).begin();
        verify(centralConnector).remove(Collections.singletonList(existing), null);
        verify(centralConnector).add(Collections.singletonList(added), null);
        verify(centralConnector).commit();
    }

    @Test
    public void commitAppliesChangesInContextsToCentralConnector() throws JenaDriverException {
        centralConnector.begin();
        final String context = Generator.generateUri().toString();
        final Statement existing = createStatement(createResource(SUBJECT), RDF.type, createResource(TYPE_ONE));
        centralConnector.add(Collections.singletonList(existing), context);
        centralConnector.commit();
        reset(centralConnector);

        connector.begin();
        connector.remove(existing.getSubject(), existing.getPredicate(), existing.getObject(), context);
        final Statement added = createStatement(createResource(SUBJECT), RDF.type, createResource(TYPE_TWO));
        connector.add(Collections.singletonList(added), context);
        connector.commit();
        verify(centralConnector).begin();
        verify(centralConnector).remove(Collections.singletonList(existing), context);
        verify(centralConnector).add(Collections.singletonList(added), context);
        verify(centralConnector).commit();
        assertFalse(connector.transaction.isActive());
    }

    @Test
    public void executeSelectQueryUsesTransactionalSnapshotToRunQuery() throws OntoDriverException {
        connector.begin();
        final Statement added = createStatement(createResource(SUBJECT), RDF.type, createResource(TYPE_ONE));
        connector.add(Collections.singletonList(added), null);
        final Query query = QueryFactory.create("SELECT * WHERE { ?x ?y ?z . }");
        try (final AbstractResultSet resultSet = connector.executeSelectQuery(query, StatementOntology.TRANSACTIONAL)) {
            assertTrue(resultSet.hasNext());
            resultSet.next();
            final String x = resultSet.getString(0);
            assertEquals(SUBJECT, x);
        }
        verify(centralConnector, never()).executeSelectQuery(eq(query), any());
    }

    @Test
    public void executeSelectQueryPassesQueryToCentralConnectorWhenConfigured() throws OntoDriverException {
        connector.begin();
        final Query query = QueryFactory.create("SELECT * WHERE { ?x ?y ?z . }");
        final AbstractResultSet resultSet = connector.executeSelectQuery(query, StatementOntology.SHARED);
        resultSet.close();
        verify(centralConnector).executeSelectQuery(query, StatementOntology.SHARED);
    }

    @Test
    public void executeAskQueryUsesTransactionalSnapshotToRunQuery() throws OntoDriverException {
        connector.begin();
        final Statement added = createStatement(createResource(SUBJECT), RDF.type, createResource(TYPE_ONE));
        connector.add(Collections.singletonList(added), null);
        final Query query = QueryFactory.create("ASK { <" + SUBJECT + "> ?y ?z . }");
        try (final AbstractResultSet resultSet = connector.executeAskQuery(query, StatementOntology.TRANSACTIONAL)) {
            assertTrue(resultSet.hasNext());
            resultSet.next();
            assertTrue(resultSet.getBoolean(0));
        }
        verify(centralConnector, never()).executeAskQuery(eq(query), any());
    }

    @Test
    public void executeAskQueryPassesQueryToCentralConnectorWhenConfigured() throws OntoDriverException {
        connector.begin();
        final Statement added = createStatement(createResource(SUBJECT), RDF.type, createResource(TYPE_ONE));
        connector.add(Collections.singletonList(added), null);
        final Query query = QueryFactory.create("ASK { <" + SUBJECT + "> ?y ?z . }");
        try (final AbstractResultSet resultSet = connector.executeAskQuery(query, StatementOntology.SHARED)) {
            assertTrue(resultSet.hasNext());
            resultSet.next();
            assertFalse(resultSet.getBoolean(0));
        }
        verify(centralConnector).executeAskQuery(eq(query), eq(StatementOntology.SHARED));
    }

    @Test
    public void executeUpdateUsesTransactionalSnapshotToRunQuery() throws OntoDriverException {
        connector.begin();
        final String update = "INSERT DATA { <" + SUBJECT + "> a <" + TYPE_ONE + "> . }";
        connector.executeUpdate(update, StatementOntology.TRANSACTIONAL);
        verify(centralConnector, never()).executeUpdate(eq(update), any());
        assertTrue(connector
                .contains(createResource(SUBJECT), RDF.type, createResource(TYPE_ONE), Collections.emptySet()));
    }

    @Test
    public void executeUpdatePassesQueryToCentralConnectorWhenConfigured() throws OntoDriverException {
        connector.begin();
        final String update = "INSERT DATA { <" + SUBJECT + "> a <" + TYPE_ONE + "> . }";
        connector.executeUpdate(update, StatementOntology.SHARED);
        verify(centralConnector).executeUpdate(update, StatementOntology.SHARED);
    }

    @Test
    public void commitExecutesQueriesRunOnTransactionalSnapshotOnCentral() throws OntoDriverException {
        connector.begin();
        final String update = "INSERT DATA { <" + SUBJECT + "> a <" + TYPE_ONE + "> . }";
        connector.executeUpdate(update, StatementOntology.TRANSACTIONAL);
        assertTrue(connector
                .contains(createResource(SUBJECT), RDF.type, createResource(TYPE_ONE), Collections.emptySet()));
        assertFalse(centralConnector
                .contains(createResource(SUBJECT), RDF.type, createResource(TYPE_ONE), Collections.emptySet()));
        connector.commit();
        verify(centralConnector).executeUpdate(update, StatementOntology.SHARED);
        assertTrue(centralConnector
                .contains(createResource(SUBJECT), RDF.type, createResource(TYPE_ONE), Collections.emptySet()));
    }

    @Test
    public void closeAfterRollbackWorksCorrectly() {
        connector.begin();
        connector.rollback();
        connector.close();
        assertFalse(connector.isOpen());
    }
}

/*
 * JOPA
 * Copyright (C) 2023 Czech Technical University in Prague
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
import cz.cvut.kbss.ontodriver.jena.environment.Generator;
import cz.cvut.kbss.ontodriver.jena.exception.JenaDriverException;
import cz.cvut.kbss.ontodriver.util.Vocabulary;
import org.apache.jena.query.Dataset;
import org.apache.jena.query.Query;
import org.apache.jena.query.QueryFactory;
import org.apache.jena.rdf.model.ResourceFactory;
import org.apache.jena.rdf.model.Statement;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Field;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Set;

import static cz.cvut.kbss.ontodriver.jena.connector.StorageTestUtil.*;
import static org.apache.jena.rdf.model.ResourceFactory.*;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

public class ChangeTrackingStorageConnectorTest {

    private SharedStorageConnector centralConnector;
    private ChangeTrackingStorageConnector connector;

    @BeforeEach
    public void setUp() {
        final DriverConfiguration configuration = StorageTestUtil.createConfiguration("test:uri");
        this.centralConnector = spy(new SharedStorageConnector(configuration));
        this.connector = new ChangeTrackingStorageConnector(centralConnector);
    }

    @AfterEach
    public void tearDown() {
        centralConnector.close();
    }

    @Test
    public void beginCreatesNewLocalModel() throws Exception {
        assertNull(getLocalModel());
        connector.begin();
        assertNotNull(getLocalModel());
        assertTrue(connector.transaction.isActive());
    }

    private LocalModel getLocalModel() throws Exception {
        final Field field = ChangeTrackingStorageConnector.class.getDeclaredField("localModel");
        field.setAccessible(true);
        return (LocalModel) field.get(connector);
    }

    @Test
    public void findEnhancesResultFromCentralConnectorWithTransactionalChangesFromLocalModel() throws Exception {
        centralConnector.begin();
        final Statement existing = ResourceFactory
                .createStatement(createResource(SUBJECT), createProperty(Vocabulary.RDF_TYPE),
                        createResource(TYPE_ONE));
        centralConnector.add(Collections.singletonList(existing), null);
        centralConnector.commit();
        connector.begin();
        final Statement added = createStatement(createResource(SUBJECT), createProperty(Vocabulary.RDF_TYPE),
                createResource(TYPE_TWO));
        getLocalModel().addStatements(Collections.singletonList(added), null);
        final Collection<Statement> result = connector
                .find(createResource(SUBJECT), null, null, Collections.emptySet());
        assertEquals(2, result.size());
        assertTrue(result.contains(existing));
        assertTrue(result.contains(added));
    }

    @Test
    public void findEnhancesResultFromCentralConnectorWithTransactionalChangesFromLocalModel_Context()
            throws Exception {
        centralConnector.begin();
        final Statement existing = ResourceFactory
                .createStatement(createResource(SUBJECT), createProperty(Vocabulary.RDF_TYPE),
                        createResource(TYPE_ONE));
        centralConnector.add(Collections.singletonList(existing), null);
        centralConnector.commit();
        connector.begin();
        final Statement added = createStatement(createResource(SUBJECT), createProperty(Vocabulary.RDF_TYPE),
                createResource(TYPE_TWO));
        getLocalModel().addStatements(Collections.singletonList(added), NAMED_GRAPH);
        final Collection<Statement> result = connector
                .find(createResource(SUBJECT), null, null, Collections.singleton(NAMED_GRAPH));
        assertEquals(1, result.size());
        assertTrue(result.contains(added));
    }

    @Test
    public void findPreventsDuplicateStatementsFromCentralAndLocalModel() throws Exception {
        centralConnector.begin();
        final Statement statement = ResourceFactory
                .createStatement(createResource(SUBJECT), createProperty(Vocabulary.RDF_TYPE),
                        createResource(TYPE_ONE));
        centralConnector.add(Collections.singletonList(statement), null);
        centralConnector.commit();
        connector.begin();
        getLocalModel().addStatements(Collections.singletonList(statement), null);
        final Collection<Statement> result = connector
                .find(createResource(SUBJECT), null, null, Collections.emptySet());
        assertEquals(1, result.size());
        assertTrue(result.contains(statement));

    }

    @Test
    public void containsReturnsTrueForStatementsPresentInLocalChanges() throws Exception {
        connector.begin();
        final Statement added = createStatement(createResource(SUBJECT), createProperty(Vocabulary.RDF_TYPE),
                createResource(TYPE_TWO));
        getLocalModel().addStatements(Collections.singletonList(added), null);
        assertTrue(connector.contains(createResource(SUBJECT), null, added.getObject(), Collections.emptySet()));
    }

    @Test
    public void containsChecksCentralConnectorWhenLocalChangesDoNotContainValue() throws Exception {
        centralConnector.begin();
        final Statement existing = ResourceFactory
                .createStatement(createResource(SUBJECT), createProperty(Vocabulary.RDF_TYPE),
                        createResource(TYPE_ONE));
        centralConnector.add(Collections.singletonList(existing), null);
        centralConnector.commit();
        connector.begin();
        assertTrue(connector.contains(createResource(SUBJECT), null, createResource(TYPE_ONE), Collections.emptySet()));
    }

    @Test
    public void containsReturnsFalseForStatementLocallyRemoved() throws Exception {
        centralConnector.begin();
        final Statement existing = ResourceFactory
                .createStatement(createResource(SUBJECT), createProperty(Vocabulary.RDF_TYPE),
                        createResource(TYPE_ONE));
        centralConnector.add(Collections.singletonList(existing), null);
        centralConnector.commit();
        connector.begin();
        final Statement removed = createStatement(createResource(SUBJECT), createProperty(Vocabulary.RDF_TYPE),
                createResource(TYPE_ONE));
        getLocalModel().removeStatements(Collections.singletonList(removed), null);
        assertFalse(connector.contains(createResource(SUBJECT), createProperty(Vocabulary.RDF_TYPE), null, Collections.emptySet()));
    }

    @Test
    public void containsWorksInContextAsWell() throws Exception {
        centralConnector.begin();
        final Statement existing = ResourceFactory
                .createStatement(createResource(SUBJECT), createProperty(Vocabulary.RDF_TYPE),
                        createResource(TYPE_ONE));
        centralConnector.add(Collections.singletonList(existing), NAMED_GRAPH);
        centralConnector.commit();
        connector.begin();
        final Statement added = createStatement(createResource(SUBJECT), createProperty(Vocabulary.RDF_TYPE),
                createResource(TYPE_TWO));
        getLocalModel().addStatements(Collections.singletonList(added), null);
        assertFalse(connector.contains(createResource(SUBJECT), null, added.getObject(), Collections.singleton(NAMED_GRAPH)));
    }

    @Test
    public void addAddsStatementsToLocalModel() throws Exception {
        final Statement statement = createStatement(createResource(SUBJECT), createProperty(Vocabulary.RDF_TYPE),
                createResource(TYPE_TWO));
        connector.begin();
        assertTrue(getLocalModel().getAdded().isEmpty());
        connector.add(Collections.singletonList(statement), null);
        assertTrue(getLocalModel().getAdded().getDefaultModel().contains(statement));
    }

    @Test
    public void addAddsStatementsToCorrectContextInLocalModel() throws Exception {
        final Statement statement = createStatement(createResource(SUBJECT), createProperty(Vocabulary.RDF_TYPE),
                createResource(TYPE_TWO));
        connector.begin();
        assertTrue(getLocalModel().getAdded().isEmpty());
        connector.add(Collections.singletonList(statement), NAMED_GRAPH);
        assertTrue(getLocalModel().getAdded().getNamedModel(NAMED_GRAPH).contains(statement));
    }

    @Test
    public void removeAddsStatementsToLocalModelForRemoval() throws Exception {
        final Statement statement = createStatement(createResource(SUBJECT), createProperty(Vocabulary.RDF_TYPE),
                createResource(TYPE_ONE));
        connector.begin();
        assertTrue(getLocalModel().getRemoved().isEmpty());
        connector.remove(Collections.singletonList(statement), null);
        assertTrue(getLocalModel().getRemoved().getDefaultModel().contains(statement));
    }

    @Test
    public void removeAddsStatementsForRemovalToCorrectContextInLocalModel() throws Exception {
        final Statement statement = createStatement(createResource(SUBJECT), createProperty(Vocabulary.RDF_TYPE),
                createResource(TYPE_ONE));
        connector.begin();
        assertTrue(getLocalModel().getRemoved().isEmpty());
        connector.remove(Collections.singletonList(statement), NAMED_GRAPH);
        assertTrue(getLocalModel().getRemoved().getNamedModel(NAMED_GRAPH).contains(statement));
    }

    @Test
    public void removeAddsStatementsMatchingRemovalCriteriaToLocalModelForRemoval() throws Exception {
        final Statement existing =
                createStatement(RESOURCE, createProperty(Vocabulary.RDF_TYPE), createResource(TYPE_ONE));
        centralConnector.begin();
        centralConnector.add(Collections.singletonList(existing), null);
        centralConnector.commit();

        connector.begin();
        assertTrue(getLocalModel().getRemoved().isEmpty());
        connector.remove(RESOURCE, createProperty(Vocabulary.RDF_TYPE), null, null);
        assertTrue(getLocalModel().getRemoved().getDefaultModel().contains(existing));
    }

    @Test
    public void removeRemovesLocallyAddedStatements() throws Exception {
        final Statement added =
                createStatement(RESOURCE, createProperty(Vocabulary.RDF_TYPE), createResource(TYPE_ONE));

        connector.begin();
        assertTrue(getLocalModel().getRemoved().isEmpty());
        getLocalModel().addStatements(Collections.singletonList(added), null);
        connector.remove(RESOURCE, createProperty(Vocabulary.RDF_TYPE), null, null);
        assertTrue(getLocalModel().getRemoved().getDefaultModel().contains(added));
        assertTrue(getLocalModel().getAdded().isEmpty());
    }

    @Test
    public void removeAddsStatementsMatchingRemovalCriteriaToLocalModelContext() throws Exception {
        final Statement existing =
                createStatement(RESOURCE, createProperty(Vocabulary.RDF_TYPE), createResource(TYPE_ONE));
        centralConnector.begin();
        centralConnector.add(Collections.singletonList(existing), NAMED_GRAPH);
        centralConnector.commit();

        connector.begin();
        assertTrue(getLocalModel().getRemoved().isEmpty());
        connector.remove(RESOURCE, createProperty(Vocabulary.RDF_TYPE), null, NAMED_GRAPH);
        assertTrue(getLocalModel().getRemoved().getNamedModel(NAMED_GRAPH).contains(existing));
    }

    @Test
    public void rollbackDiscardsLocalModel() throws Exception {
        final Statement statement = createStatement(createResource(SUBJECT), createProperty(Vocabulary.RDF_TYPE),
                createResource(TYPE_TWO));
        connector.begin();
        connector.add(Collections.singletonList(statement), null);
        connector.rollback();
        assertNull(getLocalModel());
        assertFalse(connector.transaction.isActive());
    }

    @Test
    public void commitAppliesChangesFromLocalModelToCentralConnector() throws Exception {
        final Statement removed = createStatement(createResource(SUBJECT), createProperty(Vocabulary.RDF_TYPE),
                createResource(TYPE_ONE));
        centralConnector.begin();
        centralConnector.add(Collections.singletonList(removed), null);
        centralConnector.commit();
        connector.begin();
        getLocalModel().removeStatements(Collections.singletonList(removed), null);
        final Statement added = createStatement(createResource(SUBJECT), createProperty(Vocabulary.RDF_TYPE),
                createResource(TYPE_TWO));
        getLocalModel().addStatements(Collections.singletonList(added), null);
        connector.commit();
        assertTrue(centralConnector.contains(createResource(SUBJECT), createProperty(Vocabulary.RDF_TYPE),
                createResource(TYPE_TWO), Collections.emptySet()));
        assertFalse(centralConnector.contains(createResource(SUBJECT), createProperty(Vocabulary.RDF_TYPE),
                createResource(TYPE_ONE), Collections.emptySet()));
    }

    @Test
    public void commitAppliesChangesInContexts() throws Exception {
        connector.begin();
        final Statement added = createStatement(createResource(SUBJECT), createProperty(Vocabulary.RDF_TYPE),
                createResource(TYPE_TWO));
        getLocalModel().addStatements(Collections.singletonList(added), NAMED_GRAPH);
        final SubjectPredicateContext spcToRemove = new SubjectPredicateContext(createResource(SUBJECT), createProperty(Generator.generateUri().toString()), Collections.emptySet());
        getLocalModel().removePropertyValues(Set.of(spcToRemove));
        connector.commit();
        assertTrue(centralConnector.contains(createResource(SUBJECT), createProperty(Vocabulary.RDF_TYPE),
                createResource(TYPE_TWO), Collections.singleton(NAMED_GRAPH)));
        verify(centralConnector).removePropertyValues(Set.of(spcToRemove));
    }

    @Test
    public void commitDiscardsLocalModelOnSuccessfulFinish() throws Exception {
        connector.begin();
        final Statement added = createStatement(createResource(SUBJECT), createProperty(Vocabulary.RDF_TYPE),
                createResource(TYPE_TWO));
        getLocalModel().addStatements(Collections.singletonList(added), NAMED_GRAPH);
        connector.commit();
        assertNull(getLocalModel());
        assertFalse(connector.transaction.isActive());
    }

    @Test
    public void commitRollsBackChangesOnException() throws Exception {
        connector.begin();
        final Statement added = createStatement(createResource(SUBJECT), createProperty(Vocabulary.RDF_TYPE),
                createResource(TYPE_TWO));
        getLocalModel().addStatements(Collections.singletonList(added), NAMED_GRAPH);
        centralConnector.storage = spy(centralConnector.storage);
        doThrow(new JenaDriverException("Write failed.")).when(centralConnector.storage).writeChanges();
        assertThrows(JenaDriverException.class, () -> connector.commit());
        assertFalse(centralConnector.contains(createResource(SUBJECT), createProperty(Vocabulary.RDF_TYPE),
                createResource(TYPE_TWO), Collections.singleton(NAMED_GRAPH)));
    }

    @Test
    public void closeDiscardsRunningTransaction() throws Exception {
        connector.begin();
        final Statement added = createStatement(createResource(SUBJECT), createProperty(Vocabulary.RDF_TYPE),
                createResource(TYPE_TWO));
        connector.add(Collections.singletonList(added), null);
        connector.close();
        assertNull(getLocalModel());
    }

    @Test
    public void getContextsGetsContextFromBothLocalModelAndSharedConnector() throws Exception {
        connector.begin();
        final Statement added = createStatement(createResource(SUBJECT), createProperty(Vocabulary.RDF_TYPE),
                createResource(TYPE_TWO));
        centralConnector.begin();
        centralConnector.add(Collections.singletonList(added), NAMED_GRAPH);
        centralConnector.commit();
        final String contextTwo = Generator.generateUri().toString();
        final Statement addedTwo = createStatement(createResource(SUBJECT), createProperty(Vocabulary.RDF_TYPE),
                createResource(TYPE_TWO));
        connector.add(Collections.singletonList(addedTwo), contextTwo);
        final List<String> contexts = connector.getContexts();
        assertTrue(contexts.contains(NAMED_GRAPH));
        assertTrue(contexts.contains(contextTwo));
    }

    @Test
    public void unwrapPassesCallToUnderlyingSharedConnector() {
        final Dataset dataset = connector.unwrap(Dataset.class);
        assertSame(centralConnector.storage.getDataset(), dataset);
    }

    @Test
    public void executeSelectQueryPassesQueryToSharedConnector() throws Exception {
        final Query query = QueryFactory.create("SELECT * WHERE { ?x ?y ?z . }");
        connector.executeSelectQuery(query, StatementOntology.SHARED);
        verify(centralConnector).executeSelectQuery(query, StatementOntology.SHARED);
    }

    @Test
    public void executeAskQueryPassesQueryToSharedConnector() throws Exception {
        final Query query = QueryFactory.create("ASK WHERE { ?x a <" + Generator.generateUri() + ">. }");
        connector.executeAskQuery(query, StatementOntology.SHARED);
        verify(centralConnector).executeAskQuery(query, StatementOntology.SHARED);
    }

    @Test
    public void executeUpdateQueryPassesQueryToSharedConnector() throws Exception {
        final String query = "INSERT DATA { _:a a <" + Generator.generateUri() + "> . }";
        connector.executeUpdate(query, StatementOntology.SHARED);
        verify(centralConnector).executeUpdate(query, StatementOntology.SHARED);
    }
}

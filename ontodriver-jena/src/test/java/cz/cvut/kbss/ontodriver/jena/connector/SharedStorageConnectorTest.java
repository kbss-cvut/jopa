/**
 * Copyright (C) 2022 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.jena.connector;

import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.Statement.StatementOntology;
import cz.cvut.kbss.ontodriver.config.DriverConfiguration;
import cz.cvut.kbss.ontodriver.jena.config.JenaConfigParam;
import cz.cvut.kbss.ontodriver.jena.environment.Generator;
import cz.cvut.kbss.ontodriver.jena.exception.JenaDriverException;
import org.apache.jena.graph.Graph;
import org.apache.jena.query.Dataset;
import org.apache.jena.query.Query;
import org.apache.jena.query.QueryFactory;
import org.apache.jena.rdf.model.ResourceFactory;
import org.apache.jena.rdf.model.Statement;
import org.apache.jena.vocabulary.RDF;
import org.junit.jupiter.api.Test;

import java.util.Arrays;
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

public class SharedStorageConnectorTest {

    @Test
    public void initializationCreatesStorageAccessor() {
        final SharedStorageConnector connector = initConnector();
        assertNotNull(connector.storage);
        assertTrue(connector.isOpen());
    }

    private SharedStorageConnector initConnector() {
        final DriverConfiguration configuration = StorageTestUtil.createConfiguration("test:uri");
        final SharedStorageConnector connector = new SharedStorageConnector(configuration);
        connector.storage = spy(connector.storage);
        return connector;
    }

    @Test
    public void findFiltersStatementsFromDatasetDefaultModel() {
        final SharedStorageConnector connector = initConnector();
        final Dataset ds = connector.storage.getDataset();
        generateTestData(ds);

        final Collection<Statement> result = connector.find(RESOURCE, null, null, Collections.emptySet());
        assertFalse(result.isEmpty());
    }

    @Test
    public void findInContextFiltersStatementsInTargetContext() {
        final SharedStorageConnector connector = initConnector();
        final Dataset ds = connector.storage.getDataset();
        generateTestData(ds);

        final Collection<Statement> result = connector.find(RESOURCE, null, null, Collections.singleton(NAMED_GRAPH));
        assertFalse(result.isEmpty());
    }

    @Test
    public void findInContextReturnsEmptyCollectionForUnknownContext() {
        final SharedStorageConnector connector = initConnector();
        final Dataset ds = connector.storage.getDataset();
        generateTestData(ds);
        final Collection<Statement> result = connector
                .find(RESOURCE, null, null, Collections.singleton("http://unknownGraph"));
        assertNotNull(result);
        assertTrue(result.isEmpty());
    }

    @Test
    public void containsChecksForStatementExistenceInDefaultGraph() {
        final SharedStorageConnector connector = initConnector();
        final Dataset ds = connector.unwrap(Dataset.class);
        generateTestData(ds);
        assertTrue(connector.contains(null, RDF.type, createResource(TYPE_ONE), Collections.emptySet()));
    }

    @Test
    public void containsChecksForStatementExistenceInTargetNamedGraph() {
        final SharedStorageConnector connector = initConnector();
        final Dataset ds = connector.storage.getDataset();
        generateTestData(ds);
        assertTrue(connector.contains(null, null, createResource(TYPE_TWO), Collections.singleton(NAMED_GRAPH)));
    }

    @Test
    public void closeClosesUnderlyingStorageAccessor() {
        final SharedStorageConnector connector = initConnector();
        assertTrue(connector.isOpen());
        connector.close();
        assertFalse(connector.isOpen());
        verify(connector.storage).close();
    }

    @Test
    public void addAddsStatementsToDataset() {
        final SharedStorageConnector connector = initConnector();
        final Statement statement = ResourceFactory.createStatement(RESOURCE, RDF.type, createResource(TYPE_TWO));
        assertFalse(connector.storage.getDataset().getDefaultModel().contains(statement));
        connector.begin();
        connector.add(Collections.singletonList(statement), null);
        assertTrue(connector.storage.getDataset().getDefaultModel().contains(statement));
    }

    @Test
    public void addAddsStatementsToTargetModelInDataset() {
        final SharedStorageConnector connector = initConnector();
        final Statement statement = ResourceFactory.createStatement(RESOURCE, RDF.type, createResource(TYPE_TWO));
        assertFalse(connector.storage.getDataset().getNamedModel(NAMED_GRAPH).contains(statement));
        connector.begin();
        connector.add(Collections.singletonList(statement), NAMED_GRAPH);
        assertTrue(connector.storage.getDataset().getNamedModel(NAMED_GRAPH).contains(statement));
    }

    @Test
    public void removeRemovesStatementsFromDataset() {
        final SharedStorageConnector connector = initConnector();
        final Dataset ds = connector.storage.getDataset();
        generateTestData(ds);
        final Statement statement = ResourceFactory.createStatement(RESOURCE, RDF.type, createResource(TYPE_ONE));
        assertTrue(connector.storage.getDataset().getDefaultModel().contains(statement));
        connector.begin();
        connector.remove(Collections.singletonList(statement), null);
        assertFalse(connector.storage.getDataset().getDefaultModel().contains(statement));
    }

    @Test
    public void removeRemovesStatementsFromTargetModelInDataset() {
        final SharedStorageConnector connector = initConnector();
        final Dataset ds = connector.storage.getDataset();
        generateTestData(ds);
        final Statement statement = ResourceFactory.createStatement(RESOURCE, RDF.type, createResource(TYPE_TWO));
        assertTrue(connector.storage.getDataset().getNamedModel(NAMED_GRAPH).contains(statement));
        connector.begin();
        connector.remove(Collections.singletonList(statement), NAMED_GRAPH);
        assertFalse(connector.storage.getDataset().getNamedModel(NAMED_GRAPH).contains(statement));
    }

    @Test
    public void removeRemovesStatementsMatchingSpecifiedArgumentsFromDataset() {
        final SharedStorageConnector connector = initConnector();
        final Dataset ds = connector.storage.getDataset();
        generateTestData(ds);
        assertTrue(ds.getDefaultModel().contains(RESOURCE, RDF.type, createResource(TYPE_ONE)));
        connector.begin();
        connector.remove(RESOURCE, RDF.type, null, null);
        assertFalse(ds.getDefaultModel().contains(RESOURCE, RDF.type, createResource(TYPE_ONE)));
    }

    @Test
    public void removeRemovesStatementsMatchingSpecifiedArgumentsFromTargetGraph() {
        final SharedStorageConnector connector = initConnector();
        final Dataset ds = connector.storage.getDataset();
        generateTestData(ds);
        assertTrue(ds.getNamedModel(NAMED_GRAPH).contains(RESOURCE, RDF.type, createResource(TYPE_TWO)));
        connector.begin();
        connector.remove(RESOURCE, RDF.type, null, NAMED_GRAPH);
        assertFalse(ds.getNamedModel(NAMED_GRAPH).contains(RESOURCE, RDF.type, createResource(TYPE_TWO)));
    }

    @Test
    public void beginStartsDatasetWriteTransaction() {
        final SharedStorageConnector connector = initConnector();
        connector.begin();
        assertTrue(connector.storage.getDataset().isInTransaction());
    }

    @Test
    public void commitCommitsDatasetWriteTransaction() throws Exception {
        final SharedStorageConnector connector = initConnector();
        connector.begin();
        assertTrue(connector.storage.getDataset().isInTransaction());
        final Statement statement = ResourceFactory.createStatement(RESOURCE, RDF.type, createResource(TYPE_TWO));
        connector.add(Collections.singletonList(statement), null);
        connector.commit();
        assertFalse(connector.storage.getDataset().isInTransaction());
        assertTrue(connector.storage.getDataset().getDefaultModel().contains(statement));
    }

    @Test
    public void commitWritesChangesToUnderlyingRepository() throws Exception {
        final SharedStorageConnector connector = initConnector();
        connector.begin();
        final Statement statement = ResourceFactory.createStatement(RESOURCE, RDF.type, createResource(TYPE_TWO));
        connector.add(Collections.singletonList(statement), null);
        connector.commit();
        verify(connector.storage).writeChanges();
    }

    @Test
    public void rollbackRollsBackChangesInDataset() {
        final SharedStorageConnector connector = initConnector();
        connector.begin();
        final Statement statement = ResourceFactory.createStatement(RESOURCE, RDF.type, createResource(TYPE_TWO));
        connector.add(Collections.singletonList(statement), null);
        assertTrue(connector.storage.getDataset().getDefaultModel().contains(statement));
        connector.rollback();
        assertFalse(connector.storage.getDataset().getDefaultModel().contains(statement));
    }

    @Test
    public void getContextsListsContextInRepository() throws Exception {
        final SharedStorageConnector connector = initConnector();
        connector.begin();
        final String ctxOne = Generator.generateUri().toString();
        final Statement statementOne = ResourceFactory.createStatement(RESOURCE, RDF.type, createResource(TYPE_ONE));
        final Statement statementTwo = ResourceFactory.createStatement(RESOURCE, RDF.type, createResource(TYPE_TWO));
        connector.add(Collections.singletonList(statementOne), ctxOne);
        connector.add(Collections.singletonList(statementTwo), NAMED_GRAPH);
        connector.commit();
        final List<String> contexts = connector.getContexts();
        assertTrue(contexts.contains(ctxOne));
        assertTrue(contexts.contains(NAMED_GRAPH));
    }

    @Test
    public void getContextReturnsEmptyListWhenNoNamedGraphsArePresent() {
        final SharedStorageConnector connector = initConnector();
        final List<String> contexts = connector.getContexts();
        assertNotNull(contexts);
        assertTrue(contexts.isEmpty());
    }

    @Test
    public void unwrapReturnsConnectorInstanceWhenClassMatches() {
        final SharedStorageConnector connector = initConnector();
        final StorageConnector result = connector.unwrap(StorageConnector.class);
        assertSame(connector, result);
    }

    @Test
    public void unwrapReturnsDatasetInstanceWhenClassMatches() {
        final SharedStorageConnector connector = initConnector();
        final Dataset result = connector.unwrap(Dataset.class);
        assertSame(connector.storage.getDataset(), result);
    }

    @Test
    public void unwrapThrowsUnsupportedOperationExceptionWhenTargetClassIsNotSupported() {
        final SharedStorageConnector connector = initConnector();
        assertThrows(UnsupportedOperationException.class, () -> connector.unwrap(Graph.class));
    }

    @Test
    public void removeOnDefaultDeletesStatementsFromNamedGraphWhenDefaultAsUnionIsSet() throws Exception {
        final DriverConfiguration configuration = StorageTestUtil.createConfiguration("test:uri");
        configuration.setProperty(JenaConfigParam.TREAT_DEFAULT_GRAPH_AS_UNION, Boolean.toString(true));
        final SharedStorageConnector connector = new SharedStorageConnector(configuration);
        generateTestData(connector.storage.getDataset());

        connector.begin();
        connector.remove(RESOURCE, null, null, null);
        connector.commit();
        assertFalse(connector.storage.getDataset().getDefaultModel()
                                     .contains(RESOURCE, RDF.type, createResource(TYPE_ONE)));
        assertFalse(connector.storage.getDataset().getNamedModel(NAMED_GRAPH)
                                     .contains(RESOURCE, RDF.type, createResource(TYPE_TWO)));
    }

    @Test
    public void removeOnDefaultDeletesStatementsOnlyFromDefaultWhenDefaultAsUnionIsNotSet() throws Exception {
        final SharedStorageConnector connector = initConnector();
        generateTestData(connector.storage.getDataset());

        connector.begin();
        connector.remove(RESOURCE, null, null, null);
        connector.commit();
        assertFalse(connector.storage.getDataset().getDefaultModel()
                                     .contains(RESOURCE, RDF.type, createResource(TYPE_ONE)));
        assertTrue(connector.storage.getDataset().getNamedModel(NAMED_GRAPH)
                                    .contains(RESOURCE, RDF.type, createResource(TYPE_TWO)));
    }

    @Test
    public void removeStatementsOnDefaultDeletesThemFromNamedGraphsWhenDefaultAsUnionIsSet() throws Exception {
        final DriverConfiguration configuration = StorageTestUtil.createConfiguration("test:uri");
        configuration.setProperty(JenaConfigParam.TREAT_DEFAULT_GRAPH_AS_UNION, Boolean.toString(true));
        final SharedStorageConnector connector = new SharedStorageConnector(configuration);
        generateTestData(connector.storage.getDataset());

        connector.begin();
        connector.remove(Arrays.asList(
                createStatement(RESOURCE, RDF.type, createResource(TYPE_ONE)),
                createStatement(RESOURCE, RDF.type, createResource(TYPE_TWO))
        ), null);
        connector.commit();
        assertFalse(connector.storage.getDataset().getDefaultModel()
                                     .contains(RESOURCE, RDF.type, createResource(TYPE_ONE)));
        assertFalse(connector.storage.getDataset().getNamedModel(NAMED_GRAPH)
                                     .contains(RESOURCE, RDF.type, createResource(TYPE_TWO)));
    }

    @Test
    public void removeStatementsOnDefaultRemovesThenOnlyFromDefaultWhenDefaultAsUnionIsNotSet() throws Exception {
        final SharedStorageConnector connector = initConnector();
        generateTestData(connector.storage.getDataset());

        connector.begin();
        connector.remove(Arrays.asList(
                createStatement(RESOURCE, RDF.type, createResource(TYPE_ONE)),
                createStatement(RESOURCE, RDF.type, createResource(TYPE_TWO))
        ), null);
        connector.commit();
        assertFalse(connector.storage.getDataset().getDefaultModel()
                                     .contains(RESOURCE, RDF.type, createResource(TYPE_ONE)));
        assertTrue(connector.storage.getDataset().getNamedModel(NAMED_GRAPH)
                                    .contains(RESOURCE, RDF.type, createResource(TYPE_TWO)));
    }

    @Test
    public void executeSelectQueryReturnsQueryResultSet() throws Exception {
        final SharedStorageConnector connector = initConnector();
        generateTestData(connector.storage.getDataset());
        final Query query = QueryFactory.create("SELECT * WHERE { ?x a <" + TYPE_ONE + "> . }");
        try (ResultSet result = connector.executeSelectQuery(query, StatementOntology.CENTRAL)) {
            assertNotNull(result);
            assertTrue(result.hasNext());
            result.next();
            assertEquals(SUBJECT, result.getString("x"));
        }
    }

    @Test
    public void executeSelectQueryThrowsJenaDriverExceptionWhenQueryFails() {
        final SharedStorageConnector connector = initConnector();
        generateTestData(connector.storage.getDataset());
        final Query query = QueryFactory.create("SELECT * WHERE { ?x a <" + TYPE_ONE + "> . }");
        // Causes NPX in execution
        doThrow(NullPointerException.class).when(connector.storage).prepareQuery(query);
        final JenaDriverException ex = assertThrows(JenaDriverException.class,
                () -> connector.executeSelectQuery(query, StatementOntology.CENTRAL));
        assertThat(ex.getMessage(), containsString("Execution of query " + query + " failed"));
    }

    @Test
    public void executeAskQueryReturnsQueryResultSet() throws Exception {
        final SharedStorageConnector connector = initConnector();
        generateTestData(connector.storage.getDataset());
        final Query query = QueryFactory.create("ASK WHERE { ?x a <" + TYPE_ONE + "> . }");
        final ResultSet result = connector.executeAskQuery(query, StatementOntology.CENTRAL);
        assertNotNull(result);
        assertTrue(result.hasNext());
        result.next();
        assertTrue(result.getBoolean(0));
    }

    @Test
    public void executeAskQueryThrowsJenaDriverExceptionWhenQueryFails() {
        final SharedStorageConnector connector = initConnector();
        generateTestData(connector.storage.getDataset());
        final Query query = QueryFactory.create("ASK WHERE { ?x a <" + TYPE_ONE + "> . }");
        // Causes NPX in execution
        doThrow(NullPointerException.class).when(connector.storage).prepareQuery(query);
        final JenaDriverException ex = assertThrows(JenaDriverException.class,
                () -> connector.executeAskQuery(query, StatementOntology.CENTRAL));
        assertThat(ex.getMessage(), containsString("Execution of query " + query + " failed"));
    }

    @Test
    public void executeUpdateUpdatesData() throws Exception {
        final SharedStorageConnector connector = initConnector();
        generateTestData(connector.storage.getDataset());
        final String newType = Generator.generateUri().toString();
        final String update = "INSERT DATA { <" + SUBJECT + "> a <" + newType + "> . }";
        connector.executeUpdate(update, StatementOntology.CENTRAL);
        final Collection<Statement> result = connector.find(RESOURCE, RDF.type, null, Collections.emptySet());
        assertTrue(result.stream().anyMatch(s -> s.getObject().asResource().getURI().equals(newType)));
    }

    @Test
    public void executeUpdateThrowsJenaDriverExceptionWhenQueryFails() {
        final SharedStorageConnector connector = initConnector();
        generateTestData(connector.storage.getDataset());
        final String newType = Generator.generateUri().toString();
        // Malformed query
        final String update = "INSERT DATA {" + SUBJECT + "> a <" + newType + "> . }";
        final JenaDriverException ex = assertThrows(JenaDriverException.class,
                () -> connector.executeUpdate(update, StatementOntology.CENTRAL));
        assertThat(ex.getMessage(), containsString("Execution of query " + update + " failed"));
    }

    @Test
    public void reloadStorageReloadsUnderlyingStorage() {
        final SharedStorageConnector connector = initConnector();
        connector.reloadStorage();
        verify(connector.storage).reload();
    }
}

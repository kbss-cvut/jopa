package cz.cvut.kbss.ontodriver.jena.connector;

import cz.cvut.kbss.ontodriver.Statement.StatementOntology;
import cz.cvut.kbss.ontodriver.config.Configuration;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.jena.environment.Generator;
import cz.cvut.kbss.ontodriver.jena.exception.JenaDriverException;
import cz.cvut.kbss.ontodriver.jena.query.AbstractResultSet;
import org.apache.jena.query.Dataset;
import org.apache.jena.query.Query;
import org.apache.jena.query.QueryFactory;
import org.apache.jena.rdf.model.Statement;
import org.apache.jena.vocabulary.RDF;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import java.lang.reflect.Field;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import static cz.cvut.kbss.ontodriver.jena.connector.StorageTestUtil.*;
import static org.apache.jena.rdf.model.ResourceFactory.createResource;
import static org.apache.jena.rdf.model.ResourceFactory.createStatement;
import static org.hamcrest.CoreMatchers.containsString;
import static org.junit.Assert.*;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.*;

public class SnapshotStorageConnectorTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    private SharedStorageConnector centralConnector;

    private SnapshotStorageConnector connector;

    @Before
    public void setUp() {
        final Configuration configuration = StorageTestUtil.createConfiguration("test:uri");
        this.centralConnector = spy(new SharedStorageConnector(configuration));
        this.connector = new SnapshotStorageConnector(centralConnector);
    }

    @Test
    public void beginCopiesDatasetFromCentralConnector() throws JenaDriverException {
        centralConnector.begin();
        final Statement existing = createStatement(createResource(SUBJECT), RDF.type, createResource(TYPE_ONE));
        centralConnector.add(Collections.singletonList(existing));
        centralConnector.commit();

        connector.begin();
        final Dataset dataset = connector.getStorage().getDataset();
        assertTrue(dataset.getDefaultModel().contains(existing));
        assertNotSame(centralConnector.getStorage().getDataset(), dataset);
        centralConnector.begin();
        centralConnector.remove(Collections.singletonList(existing));
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
        thrown.expect(IllegalStateException.class);
        thrown.expectMessage(containsString("Transaction is already active"));
        connector.begin();
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
        connector.add(Collections.singletonList(added));
        assertEquals(LocalModel.Containment.ADDED,
                getTransactionalChanges().contains(added.getSubject(), added.getPredicate(), added.getObject()));
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
        connector.add(Collections.singletonList(added));
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
        connector.add(Collections.singletonList(added));
        assertEquals(LocalModel.Containment.ADDED,
                getTransactionalChanges().contains(added.getSubject(), added.getPredicate(), added.getObject()));
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
                        .contains(added.getSubject(), added.getPredicate(), added.getObject(), context));
    }

    @Test
    public void findQueriesTransactionalSnapshot() {
        connector.begin();
        final Statement added = createStatement(createResource(SUBJECT), RDF.type, createResource(TYPE_ONE));
        connector.add(Collections.singletonList(added));
        final Collection<Statement> result = connector.find(added.getSubject(), added.getPredicate(), null);
        assertTrue(result.contains(added));
        verify(centralConnector, never()).find(any(), any(), any());
    }

    @Test
    public void findQueriesTransactionalSnapshotContext() {
        connector.begin();
        final String context = Generator.generateUri().toString();
        final Statement added = createStatement(createResource(SUBJECT), RDF.type, createResource(TYPE_ONE));
        connector.add(Collections.singletonList(added), context);
        final Collection<Statement> result = connector.find(added.getSubject(), added.getPredicate(), null, context);
        assertTrue(result.contains(added));
        verify(centralConnector, never()).find(any(), any(), any());
        verify(centralConnector, never()).find(any(), any(), any(), eq(context));
    }

    @Test
    public void containsQueriesTransactionalSnapshot() {
        connector.begin();
        final Statement added = createStatement(createResource(SUBJECT), RDF.type, createResource(TYPE_ONE));
        connector.add(Collections.singletonList(added));
        assertTrue(connector.contains(added.getSubject(), added.getPredicate(), null));
        verify(centralConnector, never()).contains(any(), any(), any());
    }

    @Test
    public void containsQueriesTransactionalSnapshotContext() {
        connector.begin();
        final String context = Generator.generateUri().toString();
        final Statement added = createStatement(createResource(SUBJECT), RDF.type, createResource(TYPE_ONE));
        connector.add(Collections.singletonList(added), context);
        assertTrue(connector.contains(added.getSubject(), added.getPredicate(), null, context));
        verify(centralConnector, never()).contains(any(), any(), any());
        verify(centralConnector, never()).contains(any(), any(), any(), eq(context));
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
        centralConnector.add(Collections.singletonList(existing));
        centralConnector.commit();
        connector.begin();
        assertTrue(connector.contains(existing.getSubject(), existing.getPredicate(), existing.getObject()));
        connector.remove(Collections.singletonList(existing));
        assertFalse(connector.contains(existing.getSubject(), existing.getPredicate(), existing.getObject()));
    }

    @Test
    public void removeRemovesStatementsFromSnapshotContext() throws JenaDriverException {
        centralConnector.begin();
        final String context = Generator.generateUri().toString();
        final Statement existing = createStatement(createResource(SUBJECT), RDF.type, createResource(TYPE_ONE));
        centralConnector.add(Collections.singletonList(existing), context);
        centralConnector.commit();
        connector.begin();
        assertTrue(connector.contains(existing.getSubject(), existing.getPredicate(), existing.getObject(), context));
        connector.remove(Collections.singletonList(existing), context);
        assertFalse(connector.contains(existing.getSubject(), existing.getPredicate(), existing.getObject(), context));
    }

    @Test
    public void removeRemovesStatementsFilteredBySubjectPredicateObjectFromSnapshot() throws JenaDriverException {
        centralConnector.begin();
        final Statement existing = createStatement(createResource(SUBJECT), RDF.type, createResource(TYPE_ONE));
        centralConnector.add(Collections.singletonList(existing));
        centralConnector.commit();
        connector.begin();
        assertTrue(connector.contains(existing.getSubject(), existing.getPredicate(), existing.getObject()));
        connector.remove(existing.getSubject(), existing.getPredicate(), null);
        assertFalse(connector.contains(existing.getSubject(), existing.getPredicate(), existing.getObject()));
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
        assertTrue(connector.contains(existing.getSubject(), existing.getPredicate(), existing.getObject(), context));
        connector.remove(existing.getSubject(), null, existing.getObject(), context);
        assertFalse(connector.contains(existing.getSubject(), existing.getPredicate(), existing.getObject(), context));
    }

    @Test
    public void commitAppliesChangesToCentralConnector() throws JenaDriverException {
        centralConnector.begin();
        final Statement existing = createStatement(createResource(SUBJECT), RDF.type, createResource(TYPE_ONE));
        centralConnector.add(Collections.singletonList(existing));
        centralConnector.commit();
        reset(centralConnector);

        connector.begin();
        connector.remove(existing.getSubject(), existing.getPredicate(), existing.getObject());
        final Statement added = createStatement(createResource(SUBJECT), RDF.type, createResource(TYPE_TWO));
        connector.add(Collections.singletonList(added));
        connector.commit();
        verify(centralConnector).begin();
        verify(centralConnector).remove(Collections.singletonList(existing));
        verify(centralConnector).add(Collections.singletonList(added));
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
        connector.add(Collections.singletonList(added));
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
        final AbstractResultSet resultSet = connector.executeSelectQuery(query, StatementOntology.CENTRAL);
        resultSet.close();
        verify(centralConnector).executeSelectQuery(query, StatementOntology.CENTRAL);
    }

    @Test
    public void executeAskQueryUsesTransactionalSnapshotToRunQuery() throws OntoDriverException {
        connector.begin();
        final Statement added = createStatement(createResource(SUBJECT), RDF.type, createResource(TYPE_ONE));
        connector.add(Collections.singletonList(added));
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
        connector.add(Collections.singletonList(added));
        final Query query = QueryFactory.create("ASK { <" + SUBJECT + "> ?y ?z . }");
        try (final AbstractResultSet resultSet = connector.executeAskQuery(query, StatementOntology.CENTRAL)) {
            assertTrue(resultSet.hasNext());
            resultSet.next();
            assertFalse(resultSet.getBoolean(0));
        }
        verify(centralConnector).executeAskQuery(eq(query), eq(StatementOntology.CENTRAL));
    }

    @Test
    public void executeUpdateUsesTransactionalSnapshotToRunQuery() throws OntoDriverException {
        connector.begin();
        final String update = "INSERT DATA { <" + SUBJECT + "> a <" + TYPE_ONE + "> . }";
        connector.executeUpdate(update, StatementOntology.TRANSACTIONAL);
        verify(centralConnector, never()).executeUpdate(eq(update), any());
        assertTrue(connector.contains(createResource(SUBJECT), RDF.type, createResource(TYPE_ONE)));
    }

    @Test
    public void executeUpdatePassesQueryToCentralConnectorWhenConfigured() throws OntoDriverException {
        connector.begin();
        final String update = "INSERT DATA { <" + SUBJECT + "> a <" + TYPE_ONE + "> . }";
        connector.executeUpdate(update, StatementOntology.CENTRAL);
        verify(centralConnector).executeUpdate(update, StatementOntology.CENTRAL);
    }

    @Test
    public void commitExecutesQueriesRunOnTransactionalSnapshotOnCentral() throws OntoDriverException {
        connector.begin();
        final String update = "INSERT DATA { <" + SUBJECT + "> a <" + TYPE_ONE + "> . }";
        connector.executeUpdate(update, StatementOntology.TRANSACTIONAL);
        assertTrue(connector.contains(createResource(SUBJECT), RDF.type, createResource(TYPE_ONE)));
        assertFalse(centralConnector.contains(createResource(SUBJECT), RDF.type, createResource(TYPE_ONE)));
        connector.commit();
        verify(centralConnector).executeUpdate(update, StatementOntology.CENTRAL);
        assertTrue(centralConnector.contains(createResource(SUBJECT), RDF.type, createResource(TYPE_ONE)));
    }
}
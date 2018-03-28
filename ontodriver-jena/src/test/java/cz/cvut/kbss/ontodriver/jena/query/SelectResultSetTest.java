package cz.cvut.kbss.ontodriver.jena.query;

import cz.cvut.kbss.ontodriver.jena.environment.Generator;
import cz.cvut.kbss.ontodriver.jena.exception.JenaDriverException;
import org.apache.jena.query.QueryExecution;
import org.apache.jena.query.QueryExecutionFactory;
import org.apache.jena.query.ResultSet;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.vocabulary.RDF;
import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.NoSuchElementException;

import static org.apache.jena.rdf.model.ResourceFactory.*;
import static org.hamcrest.CoreMatchers.containsString;
import static org.junit.Assert.*;
import static org.mockito.Mockito.*;

public class SelectResultSetTest {

    private static final String QUERY = "SELECT * WHERE { ?x ?y ?z . }";

    private static final Resource SUBJECT = createResource(Generator.generateUri().toString());
    private static final Resource TYPE_ONE = createResource(Generator.generateUri().toString());

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Mock
    private JenaStatement statement;

    private Model model;

    private QueryExecution execution;
    private ResultSet resultSet;
    private SelectResultSet selectResult;

    @Before
    public void setUp() {
        MockitoAnnotations.initMocks(this);
        this.model = ModelFactory.createDefaultModel();
    }

    @After
    public void tearDown() {
        if (!execution.isClosed()) {
            execution.close();
        }
    }

    private SelectResultSet resultFor(String query) {
        this.execution = spy(QueryExecutionFactory.create(query, model));
        this.resultSet = spy(execution.execSelect());
        final SelectResultSet rs = new SelectResultSet(execution, resultSet);
        rs.setStatement(statement);
        return rs;
    }

    @Test
    public void closeClosesUnderlyingJenaResultSet() throws Exception {
        this.selectResult = resultFor(QUERY);
        assertTrue(selectResult.isOpen());
        selectResult.close();
        verify(execution).close();
        assertFalse(selectResult.isOpen());
    }

    @Test
    public void findColumnReturnsIndexOfColumnWithSpecifiedName() {
        this.selectResult = resultFor(QUERY);
        assertEquals(0, selectResult.findColumn("x"));
    }

    @Test
    public void getColumnCountReturnsNumberOfBindings() {
        this.selectResult = resultFor(QUERY);
        assertEquals(3, selectResult.getColumnCount());
    }

    @Test
    public void hasNextReturnsTrueWhenResultSetHasMoreElements() {
        model.add(SUBJECT, RDF.type, TYPE_ONE);
        this.selectResult = resultFor(QUERY);
        assertTrue(selectResult.hasNext());
    }

    @Test
    public void hasNextReturnsFalseWhenNoMoreResultsAreFound() {
        this.selectResult = resultFor(QUERY);
        assertFalse(selectResult.hasNext());
    }

    @Test
    public void nextMovesResultSetCursorToNextRow() {
        model.add(SUBJECT, RDF.type, TYPE_ONE);
        this.selectResult = resultFor(QUERY);
        assertTrue(selectResult.hasNext());
        selectResult.next();
        assertFalse(selectResult.hasNext());
    }

    @Test
    public void nextThrowsNoSuchElementExceptionWhenNextIsCalledButNoMoreRowsAreAvailable() {
        model.add(SUBJECT, RDF.type, TYPE_ONE);
        this.selectResult = resultFor(QUERY);
        thrown.expect(NoSuchElementException.class);
        selectResult.next();
        selectResult.next();
    }

    @Test
    public void relativeMovesSpecifiedNumberOfRowsForward() {
        model.add(SUBJECT, RDF.type, TYPE_ONE);
        model.add(SUBJECT, RDF.type, createResource(Generator.generateUri().toString()));
        model.add(SUBJECT, RDF.type, createResource(Generator.generateUri().toString()));
        this.selectResult = resultFor(QUERY);
        selectResult.next();
        assertEquals(0, selectResult.getRowIndex());
        reset(resultSet);
        selectResult.relative(2);
        verify(resultSet, times(2)).next();
        assertEquals(2, selectResult.getRowIndex());
    }

    @Test
    public void relativeThrowsUnsupportedOperationForNegativeArgument() {
        model.add(SUBJECT, RDF.type, TYPE_ONE);
        model.add(SUBJECT, RDF.type, createResource(Generator.generateUri().toString()));
        model.add(SUBJECT, RDF.type, createResource(Generator.generateUri().toString()));
        this.selectResult = resultFor(QUERY);
        selectResult.next();
        assertEquals(0, selectResult.getRowIndex());
        thrown.expect(UnsupportedOperationException.class);
        thrown.expectMessage(containsString("Moving back is not supported by this result set."));
        selectResult.relative(-2);
    }

    @Test
    public void setRowIndexMovesToTargetRowIndex() {
        model.add(SUBJECT, RDF.type, TYPE_ONE);
        model.add(SUBJECT, RDF.type, createResource(Generator.generateUri().toString()));
        model.add(SUBJECT, RDF.type, createResource(Generator.generateUri().toString()));
        this.selectResult = resultFor(QUERY);
        selectResult.next();
        assertEquals(0, selectResult.getRowIndex());
        reset(resultSet);
        selectResult.setRowIndex(1);
        assertEquals(1, selectResult.getRowIndex());
        verify(resultSet).next();
    }

    @Test
    public void setRowIndexThrowsUnsupportedOperationForIndexLessThanCurrent() {
        model.add(SUBJECT, RDF.type, TYPE_ONE);
        model.add(SUBJECT, RDF.type, createResource(Generator.generateUri().toString()));
        model.add(SUBJECT, RDF.type, createResource(Generator.generateUri().toString()));
        this.selectResult = resultFor(QUERY);
        selectResult.next();
        selectResult.next();
        thrown.expect(UnsupportedOperationException.class);
        thrown.expectMessage(containsString("Moving back is not supported by this result set."));
        selectResult.setRowIndex(0);
    }

    @Test
    public void setRowIndexThrowsNoSuchElementWhenTargetIndexExceedsRowCount() {
        model.add(SUBJECT, RDF.type, TYPE_ONE);
        model.add(SUBJECT, RDF.type, createResource(Generator.generateUri().toString()));
        model.add(SUBJECT, RDF.type, createResource(Generator.generateUri().toString()));
        this.selectResult = resultFor(QUERY);
        thrown.expect(NoSuchElementException.class);
        selectResult.setRowIndex(10);
    }

    @Test
    public void lastMovesResultToLastExistingRow() {
        model.add(SUBJECT, RDF.type, TYPE_ONE);
        model.add(SUBJECT, RDF.type, createResource(Generator.generateUri().toString()));
        model.add(SUBJECT, RDF.type, createResource(Generator.generateUri().toString()));
        this.selectResult = resultFor(QUERY);
        assertTrue(selectResult.hasNext());
        selectResult.last();
        assertFalse(selectResult.hasNext());
    }

    @Test
    public void isFirstReturnsTrueForFirstRowInResult() {
        model.add(SUBJECT, RDF.type, TYPE_ONE);
        model.add(SUBJECT, RDF.type, createResource(Generator.generateUri().toString()));
        model.add(SUBJECT, RDF.type, createResource(Generator.generateUri().toString()));
        this.selectResult = resultFor(QUERY);
        assertFalse(selectResult.isFirst());
        selectResult.next();
        assertTrue(selectResult.isFirst());
        selectResult.next();
        assertFalse(selectResult.isFirst());
    }

    @Test
    public void getStringByIndexReturnsLiteralAsString() {
        final int value = 117;
        model.add(SUBJECT, createProperty(Generator.generateUri().toString()), createTypedLiteral(value));
        this.selectResult = resultFor(QUERY);
        selectResult.next();
        assertEquals(Integer.toString(value), selectResult.getString(2));
    }

    @Test
    public void getStringByUnknownIndexThrowsIllegalArgumentException() {
        final int value = 117;
        model.add(SUBJECT, createProperty(Generator.generateUri().toString()), createTypedLiteral(value));
        this.selectResult = resultFor(QUERY);
        selectResult.next();
        thrown.expect(IllegalArgumentException.class);
        int index = 4;
        thrown.expectMessage(containsString("Variable index " + index + " is out of bounds."));
        selectResult.getString(index);
    }

    @Test
    public void getStringByIndexReturnsResourceAsString() {
        model.add(SUBJECT, RDF.type, TYPE_ONE);
        this.selectResult = resultFor(QUERY);
        selectResult.next();
        assertEquals(TYPE_ONE.getURI(), selectResult.getString(2));
    }

    @Test
    public void getStringByVariableReturnsLiteralAsString() {
        final int value = 117;
        model.add(SUBJECT, createProperty(Generator.generateUri().toString()), createTypedLiteral(value));
        this.selectResult = resultFor(QUERY);
        selectResult.next();
        assertEquals(Integer.toString(value), selectResult.getString("z"));
    }

    @Test
    public void getStringByVariableNameThrowsIllegalArgumentExceptionForUnknownVariableName() {
        final int value = 117;
        model.add(SUBJECT, createProperty(Generator.generateUri().toString()), createTypedLiteral(value));
        this.selectResult = resultFor(QUERY);
        selectResult.next();
        final String name = "unknown";
        thrown.expect(IllegalArgumentException.class);
        thrown.expectMessage(containsString("Variable \'" + name + "\' not found in the result set."));
        selectResult.getString(name);
    }

    @Test
    public void getStringByVariableReturnsResourceAsString() {
        model.add(SUBJECT, RDF.type, TYPE_ONE);
        this.selectResult = resultFor(QUERY);
        selectResult.next();
        assertEquals(TYPE_ONE.getURI(), selectResult.getString("z"));
    }

    @Test
    public void getStringByIndexThrowsIllegalStateExceptionWhenCalledBeforeNext() {
        model.add(SUBJECT, RDF.type, TYPE_ONE);
        this.selectResult = resultFor(QUERY);
        thrown.expect(IllegalStateException.class);
        thrown.expectMessage("Must call next before getting any values.");
        selectResult.getString(0);
    }

    @Test
    public void getBooleanByIndexReturnsBooleanValue() throws JenaDriverException {
        model.add(SUBJECT, createProperty(Generator.generateUri().toString()), createTypedLiteral(true));
        this.selectResult = resultFor(QUERY);
        selectResult.next();
        assertTrue(selectResult.getBoolean(2));
    }

    @Test
    public void getBooleanByVariableReturnsBooleanValue() throws JenaDriverException {
        model.add(SUBJECT, createProperty(Generator.generateUri().toString()), createTypedLiteral(true));
        this.selectResult = resultFor(QUERY);
        selectResult.next();
        assertTrue(selectResult.getBoolean("z"));
    }

    @Test
    public void getBooleanThrowsJenaDriverExceptionWhenResourceIsEncountered() throws JenaDriverException {
        model.add(SUBJECT, RDF.type, TYPE_ONE);
        this.selectResult = resultFor(QUERY);
        selectResult.next();
        thrown.expect(JenaDriverException.class);
        thrown.expectMessage("Expected value " + RDF.type + " to be a literal.");
        selectResult.getBoolean("y");
    }
}
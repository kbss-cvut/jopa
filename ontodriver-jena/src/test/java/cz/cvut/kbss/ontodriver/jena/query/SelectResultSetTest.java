package cz.cvut.kbss.ontodriver.jena.query;

import cz.cvut.kbss.ontodriver.exception.VariableNotBoundException;
import cz.cvut.kbss.ontodriver.jena.environment.Generator;
import cz.cvut.kbss.ontodriver.jena.exception.JenaDriverException;
import org.apache.jena.query.QueryExecution;
import org.apache.jena.query.QueryExecutionFactory;
import org.apache.jena.query.ResultSet;
import org.apache.jena.rdf.model.*;
import org.apache.jena.vocabulary.RDF;
import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.net.URI;
import java.util.NoSuchElementException;

import static org.apache.jena.rdf.model.ResourceFactory.*;
import static org.hamcrest.CoreMatchers.containsString;
import static org.junit.Assert.*;
import static org.mockito.Mockito.*;

public class SelectResultSetTest {

    private static final Resource SUBJECT = createResource(Generator.generateUri().toString());
    private static final Resource TYPE_ONE = createResource(Generator.generateUri().toString());
    private static final Property PROPERTY = createProperty(Generator.generateUri().toString());

    private static final String QUERY = "SELECT * WHERE { ?x ?y ?z . }";
    private static final String OPTIONAL_QUERY =
            "SELECT ?x ?y ?z WHERE { ?x a ?y . OPTIONAL { ?x <" + PROPERTY.getURI() + "> ?z . }}";

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
        model.add(SUBJECT, PROPERTY, createTypedLiteral(value));
        this.selectResult = resultFor(QUERY);
        selectResult.next();
        assertEquals(Integer.toString(value), selectResult.getString(2));
    }

    @Test
    public void getStringByUnknownIndexThrowsIllegalArgumentException() {
        final int value = 117;
        model.add(SUBJECT, PROPERTY, createTypedLiteral(value));
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
        model.add(SUBJECT, PROPERTY, createTypedLiteral(value));
        this.selectResult = resultFor(QUERY);
        selectResult.next();
        assertEquals(Integer.toString(value), selectResult.getString("z"));
    }

    @Test
    public void getStringByVariableNameThrowsIllegalArgumentExceptionForUnknownVariableName() {
        final int value = 117;
        model.add(SUBJECT, PROPERTY, createTypedLiteral(value));
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
        saveValueAndExecuteQuery(true);
        assertTrue(selectResult.getBoolean(2));
    }

    private void saveValueAndExecuteQuery(Object value) {
        model.add(SUBJECT, PROPERTY, createTypedLiteral(value));
        this.selectResult = resultFor(QUERY);
        selectResult.next();
    }

    @Test
    public void getBooleanByVariableReturnsBooleanValue() throws JenaDriverException {
        saveValueAndExecuteQuery(true);
        assertTrue(selectResult.getBoolean("z"));
    }

    @Test
    public void getLiteralThrowsJenaDriverExceptionWhenResourceIsEncountered() throws JenaDriverException {
        model.add(SUBJECT, RDF.type, TYPE_ONE);
        this.selectResult = resultFor(QUERY);
        selectResult.next();
        thrown.expect(JenaDriverException.class);
        thrown.expectMessage("Expected value " + RDF.type + " to be a literal.");
        selectResult.getBoolean("y");
    }

    @Test
    public void getByteByIndexReturnsByteValue() throws JenaDriverException {
        saveValueAndExecuteQuery(Byte.MAX_VALUE);
        assertEquals(Byte.MAX_VALUE, selectResult.getByte(2));
    }

    @Test
    public void getByteByVariableReturnsByValue() throws JenaDriverException {
        saveValueAndExecuteQuery(Byte.MAX_VALUE);
        assertEquals(Byte.MAX_VALUE, selectResult.getByte("z"));
    }

    @Test
    public void getDoubleByIndexReturnsDoubleValue() throws JenaDriverException {
        saveValueAndExecuteQuery(Math.PI);
        assertEquals(Math.PI, selectResult.getDouble(2), 0.01);
    }

    @Test
    public void getDoubleByVariableReturnsDoubleValue() throws JenaDriverException {
        saveValueAndExecuteQuery(Math.PI);
        assertEquals(Math.PI, selectResult.getDouble("z"), 0.01);
    }

    @Test
    public void getFloatByIndexReturnsFloatValue() throws JenaDriverException {
        saveValueAndExecuteQuery(117.0f);
        assertEquals(117.0f, selectResult.getFloat(2), 0.01f);
    }

    @Test
    public void getFloatByVariableReturnsFloatValue() throws JenaDriverException {
        saveValueAndExecuteQuery(117.0f);
        assertEquals(117.0f, selectResult.getFloat("z"), 0.01f);
    }

    @Test
    public void getIntByIndexReturnsIntValue() throws JenaDriverException {
        saveValueAndExecuteQuery(117);
        assertEquals(117, selectResult.getInt(2));
    }

    @Test
    public void getIntByVariableReturnsIntValue() throws JenaDriverException {
        saveValueAndExecuteQuery(117);
        assertEquals(117, selectResult.getInt("z"));
    }

    @Test
    public void getLongByIndexReturnsLongValue() throws JenaDriverException {
        final long value = System.currentTimeMillis();
        saveValueAndExecuteQuery(value);
        assertEquals(value, selectResult.getLong(2));
    }

    @Test
    public void getLongByVariableReturnsLongValue() throws JenaDriverException {
        final long value = System.currentTimeMillis();
        saveValueAndExecuteQuery(value);
        assertEquals(value, selectResult.getLong("z"));
    }

    @Test
    public void getShortByIndexReturnsShortValue() throws JenaDriverException {
        final short value = 117;
        saveValueAndExecuteQuery(value);
        assertEquals(value, selectResult.getShort(2));
    }

    @Test
    public void getShortByVariableReturnsShortValue() throws JenaDriverException {
        final short value = 117;
        saveValueAndExecuteQuery(value);
        assertEquals(value, selectResult.getShort("z"));
    }

    @Test
    public void getShortRoundsDoubleValueToShort() throws JenaDriverException {
        saveValueAndExecuteQuery(Math.PI);
        final short result = selectResult.getShort("z");
        assertEquals((short) Math.round(Math.PI), result);
    }

    @Test
    public void getIntThrowsNumberFormatExceptionForStringValue() throws JenaDriverException {
        // This test is more for documentation purposes - it shows how Jena type resolution works
        saveValueAndExecuteQuery("Test");
        thrown.expect(NumberFormatException.class);
        selectResult.getInt(2);
    }

    @Test
    public void getObjectByIndexReturnsLiteralValue() {
        saveValueAndExecuteQuery(117);
        final Object result = selectResult.getObject(2);
        assertTrue(result instanceof Integer);
        assertEquals(117, result);
    }

    @Test
    public void getObjectByIndexReturnsURIForNamedResource() {
        model.add(SUBJECT, RDF.type, TYPE_ONE);
        this.selectResult = resultFor(QUERY);
        selectResult.next();
        final Object result = selectResult.getObject(1);
        assertTrue(result instanceof URI);
        assertEquals(URI.create(RDF.type.getURI()), result);
    }

    @Test
    public void getObjectByIndexReturnsStringValueForAnonymousResource() {
        final Resource resource = createResource();
        model.add(SUBJECT, RDF.type, resource);
        this.selectResult = resultFor(QUERY);
        selectResult.next();
        final Object result = selectResult.getObject(2);
        assertTrue(result instanceof String);
        assertEquals(resource.getId().getLabelString(), result);
    }

    @Test
    public void getObjectByVariableReturnsLiteralValue() {
        saveValueAndExecuteQuery(117);
        final Object result = selectResult.getObject("z");
        assertTrue(result instanceof Integer);
        assertEquals(117, result);
    }

    @Test
    public void getObjectByIndexAndClassReturnsObjectWhenItCanBeCastToExpectedClass() throws JenaDriverException {
        saveValueAndExecuteQuery(117);
        final Literal result = selectResult.getObject(2, Literal.class);
        assertNotNull(result);
        assertEquals(117, result.getInt());
    }

    @Test
    public void getObjectByIndexAndClassReturnsLiteralValueWhenItCanBeCastToExpectedClass() throws JenaDriverException {
        saveValueAndExecuteQuery(117);
        final Integer result = selectResult.getObject(2, Integer.class);
        assertNotNull(result);
        assertEquals(117, result.intValue());
    }

    @Test
    public void getObjectByIndexAndClassReturnsURIWhenResourceIsExtractedAndURIIsExpected() throws JenaDriverException {
        model.add(SUBJECT, RDF.type, TYPE_ONE);
        this.selectResult = resultFor(QUERY);
        selectResult.next();
        final URI result = selectResult.getObject(2, URI.class);
        assertEquals(URI.create(TYPE_ONE.getURI()), result);
    }

    @Test
    public void getObjectByIndexReturnsStringWhenBlankNodeIsExtractedAndStringIsExpected() throws JenaDriverException {
        final Resource resource = createResource();
        model.add(SUBJECT, RDF.type, resource);
        this.selectResult = resultFor(QUERY);
        selectResult.next();
        final String result = selectResult.getObject(2, String.class);
        assertEquals(resource.getId().getLabelString(), result);
    }

    @Test
    public void getObjectByIndexUsesConstructorWithJenaBasedParameterToBuildResult() throws JenaDriverException {
        saveValueAndExecuteQuery(117);
        final ResultJenaBased result = selectResult.getObject(2, ResultJenaBased.class);
        assertEquals(117, result.value);
    }

    public static class ResultJenaBased {
        private Object value;

        public ResultJenaBased(RDFNode node) {
            this.value = node.isLiteral() ? node.asLiteral().getValue() : URI.create(node.asResource().getURI());
        }
    }

    @Test
    public void getObjectByIndexUsesConstructorWithJavaBasedParameterToBuildResult() throws JenaDriverException {
        model.add(SUBJECT, RDF.type, TYPE_ONE);
        this.selectResult = resultFor(QUERY);
        selectResult.next();
        final ResultJavaBased result = selectResult.getObject(2, ResultJavaBased.class);
        assertEquals(URI.create(TYPE_ONE.getURI()), result.value);
    }

    public static class ResultJavaBased {
        private URI value;

        public ResultJavaBased(URI value) {
            this.value = value;
        }
    }

    @Test
    public void getObjectByIndexUsesStringBasedConstructorWhenPresent() throws JenaDriverException {
        final Resource resource = createResource();
        model.add(SUBJECT, RDF.type, resource);
        this.selectResult = resultFor(QUERY);
        selectResult.next();
        final ResultStringBased result = selectResult.getObject(2, ResultStringBased.class);
        assertEquals(resource.getId().getLabelString(), result.value);
    }

    public static class ResultStringBased {
        private String value;

        public ResultStringBased(String value) {
            this.value = value;
        }
    }

    @Test
    public void getObjectByIndexAndClassThrowsJenaDriverExceptionWhenNoMatchingConstructorIsFound() throws Exception {
        model.add(SUBJECT, RDF.type, TYPE_ONE);
        this.selectResult = resultFor(QUERY);
        selectResult.next();
        thrown.expect(JenaDriverException.class);
        thrown.expectMessage(
                "No suitable constructor for value " + TYPE_ONE + " found in type " + SelectResultSetTest.class);
        selectResult.getObject(2, SelectResultSetTest.class);
    }

    @Test
    public void getObjectByVariableAndClassRetrievesValueAsCorrectType() throws JenaDriverException {
        saveValueAndExecuteQuery(117);
        final ResultJenaBased result = selectResult.getObject("z", ResultJenaBased.class);
        assertEquals(117, result.value);
    }

    @Test
    public void getObjectThrowsVariableNotBoundForNonPresentOptionalValue() {
        model.add(SUBJECT, RDF.type, TYPE_ONE);
        this.selectResult = resultFor(OPTIONAL_QUERY);
        selectResult.next();
        thrown.expect(VariableNotBoundException.class);
        thrown.expectMessage(containsString("not bound in the current result row"));
        assertNull(selectResult.getObject(2));
    }

    @Test
    public void isBoundReturnsTrueForBoundVariableIndex() {
        saveValueAndExecuteQuery(117);
        assertTrue(selectResult.isBound(1));
    }

    @Test
    public void isBoundReturnsFalseForUnboundVariableIndex() {
        model.add(SUBJECT, RDF.type, TYPE_ONE);
        this.selectResult = resultFor(OPTIONAL_QUERY);
        selectResult.next();
        assertFalse(selectResult.isBound(2));
    }

    @Test
    public void isBoundReturnsTrueForBoundVariableName() {
        saveValueAndExecuteQuery(117);
        assertTrue(selectResult.isBound("x"));
    }

    @Test
    public void isBoundReturnsFalseForUnboundVariableName() {
        model.add(SUBJECT, RDF.type, TYPE_ONE);
        this.selectResult = resultFor(OPTIONAL_QUERY);
        selectResult.next();
        assertFalse(selectResult.isBound("z"));
    }
}
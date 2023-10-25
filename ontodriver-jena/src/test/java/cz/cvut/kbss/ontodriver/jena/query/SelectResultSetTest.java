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
package cz.cvut.kbss.ontodriver.jena.query;

import cz.cvut.kbss.jopa.datatype.exception.UnsupportedTypeTransformationException;
import cz.cvut.kbss.ontodriver.exception.VariableNotBoundException;
import cz.cvut.kbss.ontodriver.jena.environment.Generator;
import cz.cvut.kbss.ontodriver.jena.exception.JenaDriverException;
import org.apache.jena.query.QueryExecution;
import org.apache.jena.query.QueryExecutionFactory;
import org.apache.jena.query.ResultSet;
import org.apache.jena.rdf.model.*;
import org.apache.jena.vocabulary.RDF;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.net.URI;
import java.util.NoSuchElementException;

import static org.apache.jena.rdf.model.ResourceFactory.*;
import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class SelectResultSetTest {

    private static final Resource SUBJECT = createResource(Generator.generateUri().toString());
    private static final Resource TYPE_ONE = createResource(Generator.generateUri().toString());
    private static final Property PROPERTY = createProperty(Generator.generateUri().toString());

    private static final String QUERY = "SELECT * WHERE { ?x ?y ?z . }";
    private static final String OPTIONAL_QUERY =
            "SELECT ?x ?y ?z WHERE { ?x a ?y . OPTIONAL { ?x <" + PROPERTY.getURI() + "> ?z . }}";

    @Mock
    private JenaStatement statement;

    private Model model;

    private QueryExecution execution;
    private ResultSet resultSet;
    private SelectResultSet selectResult;

    @BeforeEach
    void setUp() {
        this.model = ModelFactory.createDefaultModel();
    }

    @AfterEach
    void tearDown() {
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
    void closeClosesUnderlyingJenaResultSet() throws Exception {
        this.selectResult = resultFor(QUERY);
        assertTrue(selectResult.isOpen());
        selectResult.close();
        verify(execution).close();
        assertFalse(selectResult.isOpen());
    }

    @Test
    void findColumnReturnsIndexOfColumnWithSpecifiedName() {
        this.selectResult = resultFor(QUERY);
        assertEquals(0, selectResult.findColumn("x"));
    }

    @Test
    void getColumnCountReturnsNumberOfBindings() {
        this.selectResult = resultFor(QUERY);
        assertEquals(3, selectResult.getColumnCount());
    }

    @Test
    void hasNextReturnsTrueWhenResultSetHasMoreElements() {
        model.add(SUBJECT, RDF.type, TYPE_ONE);
        this.selectResult = resultFor(QUERY);
        assertTrue(selectResult.hasNext());
    }

    @Test
    void hasNextReturnsFalseWhenNoMoreResultsAreFound() {
        this.selectResult = resultFor(QUERY);
        assertFalse(selectResult.hasNext());
    }

    @Test
    void nextMovesResultSetCursorToNextRow() {
        model.add(SUBJECT, RDF.type, TYPE_ONE);
        this.selectResult = resultFor(QUERY);
        assertTrue(selectResult.hasNext());
        selectResult.next();
        assertFalse(selectResult.hasNext());
    }

    @Test
    void nextThrowsNoSuchElementExceptionWhenNextIsCalledButNoMoreRowsAreAvailable() {
        model.add(SUBJECT, RDF.type, TYPE_ONE);
        this.selectResult = resultFor(QUERY);
        selectResult.next();
        assertThrows(NoSuchElementException.class, () -> selectResult.next());
    }

    @Test
    void relativeMovesSpecifiedNumberOfRowsForward() {
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
    void relativeThrowsUnsupportedOperationForNegativeArgument() {
        model.add(SUBJECT, RDF.type, TYPE_ONE);
        model.add(SUBJECT, RDF.type, createResource(Generator.generateUri().toString()));
        model.add(SUBJECT, RDF.type, createResource(Generator.generateUri().toString()));
        this.selectResult = resultFor(QUERY);
        selectResult.next();
        assertEquals(0, selectResult.getRowIndex());
        final UnsupportedOperationException ex = assertThrows(UnsupportedOperationException.class,
                () -> selectResult.relative(-2));
        assertThat(ex.getMessage(), containsString("Moving back is not supported by this result set."));
    }

    @Test
    void setRowIndexMovesToTargetRowIndex() {
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
    void setRowIndexThrowsUnsupportedOperationForIndexLessThanCurrent() {
        model.add(SUBJECT, RDF.type, TYPE_ONE);
        model.add(SUBJECT, RDF.type, createResource(Generator.generateUri().toString()));
        model.add(SUBJECT, RDF.type, createResource(Generator.generateUri().toString()));
        this.selectResult = resultFor(QUERY);
        selectResult.next();
        selectResult.next();
        final UnsupportedOperationException ex = assertThrows(UnsupportedOperationException.class,
                () -> selectResult.setRowIndex(0));
        assertThat(ex.getMessage(), containsString("Moving back is not supported by this result set."));
    }

    @Test
    void setRowIndexThrowsNoSuchElementWhenTargetIndexExceedsRowCount() {
        model.add(SUBJECT, RDF.type, TYPE_ONE);
        model.add(SUBJECT, RDF.type, createResource(Generator.generateUri().toString()));
        model.add(SUBJECT, RDF.type, createResource(Generator.generateUri().toString()));
        this.selectResult = resultFor(QUERY);
        assertThrows(NoSuchElementException.class, () -> selectResult.setRowIndex(10));
    }

    @Test
    void lastMovesResultToLastExistingRow() {
        model.add(SUBJECT, RDF.type, TYPE_ONE);
        model.add(SUBJECT, RDF.type, createResource(Generator.generateUri().toString()));
        model.add(SUBJECT, RDF.type, createResource(Generator.generateUri().toString()));
        this.selectResult = resultFor(QUERY);
        assertTrue(selectResult.hasNext());
        selectResult.last();
        assertFalse(selectResult.hasNext());
    }

    @Test
    void isFirstReturnsTrueForFirstRowInResult() {
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
    void getStringByIndexReturnsLiteralAsString() {
        final int value = 117;
        model.add(SUBJECT, PROPERTY, createTypedLiteral(value));
        this.selectResult = resultFor(QUERY);
        selectResult.next();
        assertEquals(Integer.toString(value), selectResult.getString(2));
    }

    @Test
    void getStringByUnknownIndexThrowsIllegalArgumentException() {
        final int value = 117;
        model.add(SUBJECT, PROPERTY, createTypedLiteral(value));
        this.selectResult = resultFor(QUERY);
        selectResult.next();
        int index = 4;
        final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> selectResult.getString(index));
        assertThat(ex.getMessage(), containsString("Variable index " + index + " is out of bounds."));
    }

    @Test
    void getStringByIndexReturnsResourceAsString() {
        model.add(SUBJECT, RDF.type, TYPE_ONE);
        this.selectResult = resultFor(QUERY);
        selectResult.next();
        assertEquals(TYPE_ONE.getURI(), selectResult.getString(2));
    }

    @Test
    void getStringByVariableReturnsLiteralAsString() {
        final int value = 117;
        model.add(SUBJECT, PROPERTY, createTypedLiteral(value));
        this.selectResult = resultFor(QUERY);
        selectResult.next();
        assertEquals(Integer.toString(value), selectResult.getString("z"));
    }

    @Test
    void getStringByVariableNameThrowsIllegalArgumentExceptionForUnknownVariableName() {
        final int value = 117;
        model.add(SUBJECT, PROPERTY, createTypedLiteral(value));
        this.selectResult = resultFor(QUERY);
        selectResult.next();
        final String name = "unknown";
        final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> selectResult.getString(name));
        assertThat(ex.getMessage(), containsString("Variable '" + name + "' not found in the result set."));
    }

    @Test
    void getStringByVariableReturnsResourceAsString() {
        model.add(SUBJECT, RDF.type, TYPE_ONE);
        this.selectResult = resultFor(QUERY);
        selectResult.next();
        assertEquals(TYPE_ONE.getURI(), selectResult.getString("z"));
    }

    @Test
    void getStringByIndexThrowsIllegalStateExceptionWhenCalledBeforeNext() {
        model.add(SUBJECT, RDF.type, TYPE_ONE);
        this.selectResult = resultFor(QUERY);
        final IllegalStateException ex = assertThrows(IllegalStateException.class, () -> selectResult.getString(0));
        assertEquals("Must call next before getting any values.", ex.getMessage());
    }

    @Test
    void getBooleanByIndexReturnsBooleanValue() throws JenaDriverException {
        saveValueAndExecuteQuery(true);
        assertTrue(selectResult.getBoolean(2));
    }

    private void saveValueAndExecuteQuery(Object value) {
        model.add(SUBJECT, PROPERTY, createTypedLiteral(value));
        this.selectResult = resultFor(QUERY);
        selectResult.next();
    }

    @Test
    void getBooleanByVariableReturnsBooleanValue() throws JenaDriverException {
        saveValueAndExecuteQuery(true);
        assertTrue(selectResult.getBoolean("z"));
    }

    @Test
    void getLiteralThrowsJenaDriverExceptionWhenResourceIsEncountered() {
        model.add(SUBJECT, RDF.type, TYPE_ONE);
        this.selectResult = resultFor(QUERY);
        selectResult.next();
        final JenaDriverException ex = assertThrows(JenaDriverException.class, () -> selectResult.getBoolean("y"));
        assertEquals("Expected value " + RDF.type + " to be a literal.", ex.getMessage());

    }

    @Test
    void getByteByIndexReturnsByteValue() throws JenaDriverException {
        saveValueAndExecuteQuery(Byte.MAX_VALUE);
        assertEquals(Byte.MAX_VALUE, selectResult.getByte(2));
    }

    @Test
    void getByteByVariableReturnsByValue() throws JenaDriverException {
        saveValueAndExecuteQuery(Byte.MAX_VALUE);
        assertEquals(Byte.MAX_VALUE, selectResult.getByte("z"));
    }

    @Test
    void getDoubleByIndexReturnsDoubleValue() throws JenaDriverException {
        saveValueAndExecuteQuery(Math.PI);
        assertEquals(Math.PI, selectResult.getDouble(2), 0.01);
    }

    @Test
    void getDoubleByVariableReturnsDoubleValue() throws JenaDriverException {
        saveValueAndExecuteQuery(Math.PI);
        assertEquals(Math.PI, selectResult.getDouble("z"), 0.01);
    }

    @Test
    void getFloatByIndexReturnsFloatValue() throws JenaDriverException {
        saveValueAndExecuteQuery(117.0f);
        assertEquals(117.0f, selectResult.getFloat(2), 0.01f);
    }

    @Test
    void getFloatByVariableReturnsFloatValue() throws JenaDriverException {
        saveValueAndExecuteQuery(117.0f);
        assertEquals(117.0f, selectResult.getFloat("z"), 0.01f);
    }

    @Test
    void getIntByIndexReturnsIntValue() throws JenaDriverException {
        saveValueAndExecuteQuery(117);
        assertEquals(117, selectResult.getInt(2));
    }

    @Test
    void getIntByVariableReturnsIntValue() throws JenaDriverException {
        saveValueAndExecuteQuery(117);
        assertEquals(117, selectResult.getInt("z"));
    }

    @Test
    void getLongByIndexReturnsLongValue() throws JenaDriverException {
        final long value = System.currentTimeMillis();
        saveValueAndExecuteQuery(value);
        assertEquals(value, selectResult.getLong(2));
    }

    @Test
    void getLongByVariableReturnsLongValue() throws JenaDriverException {
        final long value = System.currentTimeMillis();
        saveValueAndExecuteQuery(value);
        assertEquals(value, selectResult.getLong("z"));
    }

    @Test
    void getShortByIndexReturnsShortValue() throws JenaDriverException {
        final short value = 117;
        saveValueAndExecuteQuery(value);
        assertEquals(value, selectResult.getShort(2));
    }

    @Test
    void getShortByVariableReturnsShortValue() throws JenaDriverException {
        final short value = 117;
        saveValueAndExecuteQuery(value);
        assertEquals(value, selectResult.getShort("z"));
    }

    @Test
    void getShortRoundsDoubleValueToShort() throws JenaDriverException {
        saveValueAndExecuteQuery(Math.PI);
        final short result = selectResult.getShort("z");
        assertEquals((short) Math.round(Math.PI), result);
    }

    @Test
    void getIntThrowsNumberFormatExceptionForStringValue() {
        // This test is more for documentation purposes - it shows how Jena type resolution works
        saveValueAndExecuteQuery("Test");
        assertThrows(NumberFormatException.class, () -> selectResult.getInt(2));
    }

    @Test
    void getObjectByIndexReturnsLiteralValue() {
        saveValueAndExecuteQuery(117);
        final Object result = selectResult.getObject(2);
        assertTrue(result instanceof Integer);
        assertEquals(117, result);
    }

    @Test
    void getObjectByIndexReturnsURIForNamedResource() {
        model.add(SUBJECT, RDF.type, TYPE_ONE);
        this.selectResult = resultFor(QUERY);
        selectResult.next();
        final Object result = selectResult.getObject(1);
        assertTrue(result instanceof URI);
        assertEquals(URI.create(RDF.type.getURI()), result);
    }

    @Test
    void getObjectByIndexReturnsStringValueForAnonymousResource() {
        final Resource resource = createResource();
        model.add(SUBJECT, RDF.type, resource);
        this.selectResult = resultFor(QUERY);
        selectResult.next();
        final Object result = selectResult.getObject(2);
        assertTrue(result instanceof String);
        assertEquals(resource.getId().getLabelString(), result);
    }

    @Test
    void getObjectByVariableReturnsLiteralValue() {
        saveValueAndExecuteQuery(117);
        final Object result = selectResult.getObject("z");
        assertTrue(result instanceof Integer);
        assertEquals(117, result);
    }

    @Test
    void getObjectByIndexAndClassReturnsObjectWhenItCanBeCastToExpectedClass() throws JenaDriverException {
        saveValueAndExecuteQuery(117);
        final Literal result = selectResult.getObject(2, Literal.class);
        assertNotNull(result);
        assertEquals(117, result.getInt());
    }

    @Test
    void getObjectByIndexAndClassReturnsLiteralValueWhenItCanBeCastToExpectedClass() throws JenaDriverException {
        saveValueAndExecuteQuery(117);
        final Integer result = selectResult.getObject(2, Integer.class);
        assertNotNull(result);
        assertEquals(117, result.intValue());
    }

    @Test
    void getObjectByIndexAndClassReturnsURIWhenResourceIsExtractedAndURIIsExpected() throws JenaDriverException {
        model.add(SUBJECT, RDF.type, TYPE_ONE);
        this.selectResult = resultFor(QUERY);
        selectResult.next();
        final URI result = selectResult.getObject(2, URI.class);
        assertEquals(URI.create(TYPE_ONE.getURI()), result);
    }

    @Test
    void getObjectByIndexReturnsStringWhenBlankNodeIsExtractedAndStringIsExpected() throws JenaDriverException {
        final Resource resource = createResource();
        model.add(SUBJECT, RDF.type, resource);
        this.selectResult = resultFor(QUERY);
        selectResult.next();
        final String result = selectResult.getObject(2, String.class);
        assertEquals(resource.getId().getLabelString(), result);
    }

    @Test
    void getObjectByIndexUsesConstructorWithJenaBasedParameterToBuildResult() throws JenaDriverException {
        saveValueAndExecuteQuery(117);
        final ResultJenaBased result = selectResult.getObject(2, ResultJenaBased.class);
        assertEquals(117, result.value);
    }

    public static class ResultJenaBased {
        private final Object value;

        public ResultJenaBased(Object value) {
            this.value = value;
        }
    }

    @Test
    void getObjectByIndexUsesConstructorWithJavaBasedParameterToBuildResult() throws JenaDriverException {
        model.add(SUBJECT, RDF.type, TYPE_ONE);
        this.selectResult = resultFor(QUERY);
        selectResult.next();
        final ResultJavaBased result = selectResult.getObject(2, ResultJavaBased.class);
        assertEquals(URI.create(TYPE_ONE.getURI()), result.value);
    }

    public static class ResultJavaBased {
        private final URI value;

        public ResultJavaBased(URI value) {
            this.value = value;
        }
    }

    @Test
    void getObjectByIndexUsesStringBasedConstructorWhenPresent() throws JenaDriverException {
        final Resource resource = createResource();
        model.add(SUBJECT, RDF.type, resource);
        this.selectResult = resultFor(QUERY);
        selectResult.next();
        final ResultStringBased result = selectResult.getObject(2, ResultStringBased.class);
        assertEquals(resource.getId().getLabelString(), result.value);
    }

    public static class ResultStringBased {
        private final String value;

        public ResultStringBased(String value) {
            this.value = value;
        }
    }

    @Test
    void getObjectByIndexAndClassThrowsUnsupportedTypeTransformationExceptionWhenNoMatchingConstructorIsFound() {
        model.add(SUBJECT, RDF.type, TYPE_ONE);
        this.selectResult = resultFor(QUERY);
        selectResult.next();
        assertThrows(UnsupportedTypeTransformationException.class,
                () -> selectResult.getObject(2, SelectResultSetTest.class));
    }

    @Test
    void getObjectByVariableAndClassRetrievesValueAsCorrectType() throws JenaDriverException {
        saveValueAndExecuteQuery(117);
        final ResultJenaBased result = selectResult.getObject("z", ResultJenaBased.class);
        assertEquals(117, result.value);
    }

    @Test
    void getObjectThrowsVariableNotBoundForNonPresentOptionalValue() {
        model.add(SUBJECT, RDF.type, TYPE_ONE);
        this.selectResult = resultFor(OPTIONAL_QUERY);
        selectResult.next();
        final VariableNotBoundException ex = assertThrows(VariableNotBoundException.class,
                () -> selectResult.getObject(2));
        assertThat(ex.getMessage(), containsString("not bound in the current result row"));
    }

    @Test
    void isBoundReturnsTrueForBoundVariableIndex() {
        saveValueAndExecuteQuery(117);
        assertTrue(selectResult.isBound(1));
    }

    @Test
    void isBoundReturnsFalseForUnboundVariableIndex() {
        model.add(SUBJECT, RDF.type, TYPE_ONE);
        this.selectResult = resultFor(OPTIONAL_QUERY);
        selectResult.next();
        assertFalse(selectResult.isBound(2));
    }

    @Test
    void isBoundReturnsTrueForBoundVariableName() {
        saveValueAndExecuteQuery(117);
        assertTrue(selectResult.isBound("x"));
    }

    @Test
    void isBoundReturnsFalseForUnboundVariableName() {
        model.add(SUBJECT, RDF.type, TYPE_ONE);
        this.selectResult = resultFor(OPTIONAL_QUERY);
        selectResult.next();
        assertFalse(selectResult.isBound("z"));
    }

    @Test
    void getStringByIndexReturnsIdentifierForBlankNodeResult() {
        model.add(SUBJECT, PROPERTY, model.createResource());
        this.selectResult = resultFor(QUERY);
        selectResult.next();
        assertNotNull(selectResult.getString(2));
    }

    @Test
    void getStringByVariableNameReturnsIdentifierForBlankNodeResult() {
        model.add(SUBJECT, PROPERTY, model.createResource());
        this.selectResult = resultFor(QUERY);
        selectResult.next();
        assertNotNull(selectResult.getString("z"));
    }
}

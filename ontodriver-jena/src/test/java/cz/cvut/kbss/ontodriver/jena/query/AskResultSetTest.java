/**
 * Copyright (C) 2019 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.jena.query;

import cz.cvut.kbss.ontodriver.jena.exception.JenaDriverException;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import java.util.NoSuchElementException;

import static org.hamcrest.CoreMatchers.containsString;
import static org.junit.Assert.*;

/**
 * Only getBoolean, getString and getObject with compatible type are supported, other get value methods are unsupported
 * for ASK results.
 */
public class AskResultSetTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    private AskResultSet resultSet = new AskResultSet(true);

    @Test
    public void hasNextReturnsTrueOnlyBeforeFirstNextCall() {
        assertTrue(resultSet.hasNext());
        resultSet.next();
        assertFalse(resultSet.hasNext());
    }

    @Test
    public void nextMovesResultSetToFirstRow() {
        resultSet.next();
        assertEquals(0, resultSet.getRowIndex());
    }

    @Test
    public void nextThrowsNoSuchElementExceptionWhenCalledAfterFirstRow() {
        resultSet.next();
        thrown.expect(NoSuchElementException.class);
        resultSet.next();
    }

    @Test
    public void getColumnCountReturnsOne() {
        assertEquals(1, resultSet.getColumnCount());
    }

    @Test
    public void findColumnReturnsZeroForAnyName() {
        assertEquals(0, resultSet.findColumn("x"));
        assertEquals(0, resultSet.findColumn("result"));
        assertEquals(0, resultSet.findColumn("whatever"));
    }

    @Test
    public void getBooleanByIndexReturnsResult() {
        resultSet.next();
        assertTrue(resultSet.getBoolean(0));
    }

    @Test
    public void getBooleanByVariableReturnsResult() {
        resultSet.next();
        assertTrue(resultSet.getBoolean("x"));
    }

    @Test
    public void getStringByIndexReturnsStringRepresentationOfResult() {
        resultSet.next();
        assertEquals(Boolean.TRUE.toString(), resultSet.getString(0));
    }

    @Test
    public void getStringByVariableReturnsStringRepresentationOfResult() {
        resultSet.next();
        assertEquals(Boolean.TRUE.toString(), resultSet.getString("result"));
    }

    @Test
    public void getObjectByIndexReturnsResultBoxed() {
        resultSet.next();
        final Object result = resultSet.getObject(0);
        assertTrue(result instanceof Boolean);
        assertTrue((Boolean) result);
    }

    @Test
    public void getObjectByVariableReturnsResultBoxed() {
        resultSet.next();
        final Object result = resultSet.getObject("result");
        assertTrue(result instanceof Boolean);
        assertTrue((Boolean) result);
    }

    @Test
    public void getObjectByClassReturnsValueCastToBoolean() throws Exception {
        resultSet.next();
        final Boolean result = resultSet.getObject(0, Boolean.class);
        assertTrue(result);
    }

    @Test
    public void getObjectByClassReturnsValueAsString() throws Exception {
        resultSet.next();
        final String result = resultSet.getObject("x", String.class);
        assertEquals(Boolean.TRUE.toString(), result);
    }

    @Test
    public void getObjectByClassConstructsObjectUsingConstructorWithBooleanParameter() throws Exception {
        resultSet.next();
        final WithBooleanParameter result = resultSet.getObject(0, WithBooleanParameter.class);
        assertTrue(result.value);
    }

    public static class WithBooleanParameter {
        private final boolean value;

        public WithBooleanParameter(boolean value) {
            this.value = value;
        }
    }

    @Test
    public void getObjectByClassConstructsObjectUsingConstructorWithStringParameter() throws Exception {
        resultSet.next();
        final WithStringParameter result = resultSet.getObject("x", WithStringParameter.class);
        assertTrue(result.value);
    }

    public static class WithStringParameter {
        private final boolean value;

        public WithStringParameter(String value) {
            this.value = Boolean.parseBoolean(value);
        }
    }

    @Test
    public void getObjectByClassThrowsJenaDriverExceptionWhenNoSuitableConstructorIsFound() throws Exception {
        resultSet.next();
        thrown.expect(JenaDriverException.class);
        thrown.expectMessage(containsString("No suitable constructor for value "));
        thrown.expectMessage(containsString("found in type "));
        resultSet.getObject(0, AskResultSetTest.class);
    }

    @Test
    public void getByteByIndexThrowsUnsupportedOperationException() {
        resultSet.next();
        expectUnsupportedOperation("byte");
        resultSet.getByte(0);
    }

    private void expectUnsupportedOperation(String type) {
        thrown.expect(UnsupportedOperationException.class);
        thrown.expectMessage(containsString("ASK query results cannot return"));
        thrown.expectMessage(containsString(type));
    }

    @Test
    public void getByteByVariableThrowsUnsupportedOperationException() {
        resultSet.next();
        expectUnsupportedOperation("byte");
        resultSet.getByte("x");
    }

    @Test
    public void getIntByIndexThrowsUnsupportedOperationException() {
        resultSet.next();
        expectUnsupportedOperation("int");
        resultSet.getInt(0);
    }

    @Test
    public void getIntByVariableThrowsUnsupportedOperationException() {
        resultSet.next();
        expectUnsupportedOperation("int");
        resultSet.getInt("result");
    }

    @Test
    public void getLongByIndexThrowsUnsupportedOperationException() {
        resultSet.next();
        expectUnsupportedOperation("long");
        resultSet.getLong(0);
    }

    @Test
    public void getLongByVariableThrowsUnsupportedOperationException() {
        resultSet.next();
        expectUnsupportedOperation("long");
        resultSet.getLong("x");
    }

    @Test
    public void getFloatByIndexThrowsUnsupportedOperationException() {
        resultSet.next();
        expectUnsupportedOperation("float");
        resultSet.getFloat(0);
    }

    @Test
    public void getFloatByVariableThrowsUnsupportedOperationException() {
        resultSet.next();
        expectUnsupportedOperation("float");
        resultSet.getFloat("x");
    }

    @Test
    public void getDoubleByIndexThrowsUnsupportedOperationException() {
        resultSet.next();
        expectUnsupportedOperation("double");
        resultSet.getDouble(0);
    }

    @Test
    public void getDoubleByVariableThrowsUnsupportedOperationException() {
        resultSet.next();
        expectUnsupportedOperation("double");
        resultSet.getDouble("result");
    }

    @Test
    public void getShortByIndexThrowsUnsupportedOperationException() {
        resultSet.next();
        expectUnsupportedOperation("short");
        resultSet.getShort(0);
    }

    @Test
    public void getShortByVariableThrowsUnsupportedOperationException() {
        resultSet.next();
        expectUnsupportedOperation("short");
        resultSet.getShort("x");
    }
}
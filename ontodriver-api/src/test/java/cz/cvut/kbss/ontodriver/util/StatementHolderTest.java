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
package cz.cvut.kbss.ontodriver.util;

import org.junit.jupiter.api.Test;

import java.lang.reflect.Field;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

class StatementHolderTest {

    @Test
    void constructorThrowsExceptionForNullStatement() {
        assertThrows(NullPointerException.class, () -> new StatementHolder(null));
    }

    @Test
    void analyzingStatementWithoutParametersDoesNothing() {
        final String query = "SELECT nothing WHERE { <http://subject> <http://property> <http://subject> . }";
        final StatementHolder statementHolder = new StatementHolder(query);
        statementHolder.analyzeStatement();
        assertEquals(query, statementHolder.assembleStatement());
    }

    @Test
    void testAnalyzeSimpleQueryWithTwoParams() throws Exception {
        final String query = "SELECT ?x ?y WHERE { ?x <http://property> ?y . }";
        final StatementHolder holder = new StatementHolder(query);
        holder.analyzeStatement();
        final Field paramNamesField = StatementHolder.class.getDeclaredField("paramNames");
        paramNamesField.setAccessible(true);
        final List<String> paramNames = (List<String>) paramNamesField.get(holder);
        assertTrue(paramNames.contains("x"));
        assertTrue(paramNames.contains("y"));
    }

    @Test
    void analyzeQueryHandlesParameterFollowedImmediatelyByFullStop() throws Exception {
        final String query = "SELECT * WHERE { ?x ?y ?z. }";
        final StatementHolder holder = new StatementHolder(query);
        holder.analyzeStatement();
        assertEquals(query, holder.assembleStatement());
        final Field paramNamesField = StatementHolder.class.getDeclaredField("paramNames");
        paramNamesField.setAccessible(true);
        final List<String> paramNames = (List<String>) paramNamesField.get(holder);
        assertTrue(paramNames.contains("x"));
        assertTrue(paramNames.contains("y"));
        assertTrue(paramNames.contains("z"));
    }

    @Test
    void testAssembleSimpleQueryWithTwoParams() {
        final String query = "SELECT ?x WHERE { ?x <http://property> ?y . }";
        final String expected = "SELECT ?x WHERE { ?x <http://property> 'Bill' . }";
        final StatementHolder holder = new StatementHolder(query);
        holder.analyzeStatement();
        holder.setParameter("y", "'Bill'");
        assertEquals(expected, holder.assembleStatement());
    }

    @Test
    void testQueryWithNewlines() {
        final String query = "WITH <urn:sparql:tests:update:insert:delete:with>"
                + "DELETE { ?person <http://xmlns.com/foaf/0.1/givenName> ?name }"
                + "INSERT { ?person <http://xmlns.com/foaf/0.1/givenName> 'William' } WHERE\n"
                + "{\n?person <http://xmlns.com/foaf/0.1/givenName> ?name\n}";
        final String expected = "WITH <urn:sparql:tests:update:insert:delete:with>"
                + "DELETE { ?person <http://xmlns.com/foaf/0.1/givenName> 'Bill' }"
                + "INSERT { ?person <http://xmlns.com/foaf/0.1/givenName> 'William' } WHERE\n"
                + "{\n?person <http://xmlns.com/foaf/0.1/givenName> 'Bill'\n}";
        final StatementHolder holder = new StatementHolder(query);
        holder.analyzeStatement();
        holder.setParameter("name", "'Bill'");
        assertEquals(expected, holder.assembleStatement());
    }

    @Test
    void testQueryWithDoubleQuotes() {
        final String query = "WITH <urn:sparql:tests:update:insert:delete:with>"
                + "DELETE { ?person <http://xmlns.com/foaf/0.1/givenName> ?name }"
                + "INSERT { ?person <http://xmlns.com/foaf/0.1/givenName> \"William\" } WHERE\n"
                + "{\n?person <http://xmlns.com/foaf/0.1/givenName> ?name\n}";
        final String expected = "WITH <urn:sparql:tests:update:insert:delete:with>"
                + "DELETE { ?person <http://xmlns.com/foaf/0.1/givenName> \"Bill\" }"
                + "INSERT { ?person <http://xmlns.com/foaf/0.1/givenName> \"William\" } WHERE\n"
                + "{\n?person <http://xmlns.com/foaf/0.1/givenName> \"Bill\"\n}";
        final StatementHolder holder = new StatementHolder(query);
        holder.analyzeStatement();
        holder.setParameter("name", "\"Bill\"");
        assertEquals(expected, holder.assembleStatement());
    }

    @Test
    void clearParametersRemovesAllPreviouslySetParameterValues() throws Exception {
        final String query = "SELECT ?x ?y WHERE { ?x <http://property> ?y . }";
        final StatementHolder holder = new StatementHolder(query);
        holder.analyzeStatement();
        final Field paramValuesField = StatementHolder.class.getDeclaredField("paramValues");
        paramValuesField.setAccessible(true);
        final Map<String, String> paramValues = (Map<String, String>) paramValuesField.get(holder);
        final String p1 = "_:subject";
        final String p2 = "<http://object>";
        holder.setParameter("x", p1);
        holder.setParameter("y", p2);
        assertEquals(p1, paramValues.get("x"));
        assertEquals(p2, paramValues.get("y"));
        holder.clearParameters();
        assertNull(paramValues.get("x"));
        assertNull(paramValues.get("y"));
    }

    @Test
    void throwsExceptionWhenTryingToSetParameterOnStatementNotAnalysed() {
        final String query = "SELECT ?x ?y WHERE { ?x <http://property> ?y . }";
        final StatementHolder holder = new StatementHolder(query);
        assertThrows(IllegalStateException.class, () -> holder.setParameter("x", "'hey'"));
    }

    @Test
    void setUnknownParameterValueThrowsException() {
        final String query = "SELECT ?x ?y WHERE { ?x <http://property> ?y . }";
        final StatementHolder holder = new StatementHolder(query);
        holder.analyzeStatement();
        assertThrows(IllegalArgumentException.class, () -> holder.setParameter("z", "someValue@en"));
    }
}
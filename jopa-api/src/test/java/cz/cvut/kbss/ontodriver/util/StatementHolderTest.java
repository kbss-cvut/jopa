package cz.cvut.kbss.ontodriver.util;

import org.junit.Test;

import java.lang.reflect.Field;
import java.util.List;
import java.util.Map;

import static org.junit.Assert.*;

public class StatementHolderTest {

    @Test(expected = NullPointerException.class)
    public void constructorThrowsExceptionForNullStatement() throws Exception {
        new StatementHolder(null);
    }

    @Test
    public void analyzingStatementWithoutParametersDoesNothing() throws Exception {
        final String query = "SELECT nothing WHERE { <http://subject> <http://property> <http://subject> . }";
        final StatementHolder statementHolder = new StatementHolder(query);
        statementHolder.analyzeStatement();
        assertEquals(query, statementHolder.assembleStatement());
    }

    @Test
    public void testAnalyzeSimpleQueryWithTwoParams() throws Exception {
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
    public void testAssembleSimpleQueryWithTwoParams() throws Exception {
        final String query = "SELECT ?x WHERE { ?x <http://property> ?y . }";
        final String expected = "SELECT ?x WHERE { ?x <http://property> 'Bill' . }";
        final StatementHolder holder = new StatementHolder(query);
        holder.analyzeStatement();
        holder.setParameter("y", "'Bill'");
        assertEquals(expected, holder.assembleStatement());
    }

    @Test
    public void testQueryWithNewlines() throws Exception {
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
    public void testQueryWithDoubleQuotes() throws Exception {
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
    public void clearParametersRemovesAllPreviouslySetParameterValues() throws Exception {
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

    @Test(expected = IllegalStateException.class)
    public void throwsExceptionWhenTryingToSetParameterOnStatementNotAnalysed() throws Exception {
        final String query = "SELECT ?x ?y WHERE { ?x <http://property> ?y . }";
        final StatementHolder holder = new StatementHolder(query);
        holder.setParameter("x", "'hey'");
    }

    @Test(expected = IllegalArgumentException.class)
    public void setUnknownParameterValueThrowsException() throws Exception {
        final String query = "SELECT ?x ?y WHERE { ?x <http://property> ?y . }";
        final StatementHolder holder = new StatementHolder(query);
        holder.analyzeStatement();
        holder.setParameter("z", "someValue@en");
    }
}
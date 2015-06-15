package cz.cvut.kbss.jopa.query.impl;

import cz.cvut.kbss.jopa.query.ParameterValue;
import org.junit.Before;
import org.junit.Test;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.*;

import static org.junit.Assert.*;

/**
 * @author kidney
 */
public class SparqlQueryHolderTest {

    private static final List<String> PARTS = Arrays.asList("PREFIX foaf: <http://xmlns.com/foaf/0.1/>\n" +
            "SELECT ", "\n{\n", " foaf:name \"Apollo 7\" .\n", " foaf:homepage ", "\n}");
    private static final List<String> PARAMS = Arrays.asList("craft", "craft", "craft", "homepage");
    private static final Set<String> PARAM_NAMES = new HashSet<>(Arrays.asList("craft", "homepage"));

    private SparqlQueryHolder holder;

    @Before
    public void setUp() throws Exception {
        this.holder = new SparqlQueryHolder(PARTS, PARAMS);
    }

    @Test
    public void testGetParameters() throws Exception {
        final Collection<String> res = holder.getParameters();
        assertEquals(PARAM_NAMES.size(), res.size());
        assertTrue(PARAM_NAMES.containsAll(res));
    }

    @Test
    public void testGetParameterValue() throws Exception {
        final String value = "http://kbss.felk.cvut.cz";
        holder.setParameter("homepage", new StringParameterValue(value));
        assertEquals(value, holder.getParameterValue("homepage"));
    }

    @Test(expected = IllegalArgumentException.class)
    public void getValueOfUnknownParameterThrowsIllegalArgumentException() throws Exception {
        holder.setParameter("homepage", new StringParameterValue("tatata"));
        holder.getParameterValue("blabla");
    }

    @Test
    public void testSetParameter() throws Exception {
        final String value = "http://kbss.felk.cvut.cz";
        final String expected = "\"" + value + "\"";
        holder.setParameter("homepage", new StringParameterValue(value));
        assertEquals(expected, getParameter("homepage"));
    }

    private String getParameter(String name) throws Exception {
        final Field valuesField = SparqlQueryHolder.class.getDeclaredField("values");
        valuesField.setAccessible(true);
        final Map<String, ParameterValue> values = getParamValues();
        return values.get(name).getQueryString();
    }

    @Test(expected = NullPointerException.class)
    public void setParameterToNullThrowsException() throws Exception {
        holder.setParameter("homepage", null);
    }

    @Test(expected = NullPointerException.class)
    public void setNullParameterThrowsException() throws Exception {
        holder.setParameter(null, new StringParameterValue("Whatever"));
    }

    @Test(expected = IllegalArgumentException.class)
    public void setUnknownParameterThrowsException() throws Exception {
        holder.setParameter("unknownParameter", new StringParameterValue("Whatever"));
    }

    @Test
    public void clearParameterRemovesParameterValue() throws Exception {
        holder.setParameter("homepage", new UriParameterValue(URI.create("http://kbss.felk.cvut.cz")));
        final Map<String, ParameterValue> paramValues = getParamValues();
        assertTrue(paramValues.containsKey("homepage"));
        holder.clearParameter("homepage");
        assertFalse(paramValues.containsKey("homepage"));
    }

    private Map<String, ParameterValue> getParamValues() throws Exception {
        final Field field = SparqlQueryHolder.class.getDeclaredField("values");
        field.setAccessible(true);
        return (Map<String, ParameterValue>) field.get(holder);
    }

    @Test
    public void clearParametersRemovesAllParameterValues() throws Exception {
        holder.setParameter("homepage", new UriParameterValue(URI.create("http://kbss.felk.cvut.cz")));
        final Map<String, ParameterValue> paramValues = getParamValues();
        assertFalse(paramValues.isEmpty());
        holder.clearParameters();
        assertTrue(paramValues.isEmpty());
    }

    @Test
    public void assembleQueryWithUri() throws Exception {
        holder.setParameter("homepage", new UriParameterValue(URI.create("http://kbss.felk.cvut.cz")));
        final String expected = "PREFIX foaf: <http://xmlns.com/foaf/0.1/>\n" +
                "SELECT ?craft\n" +
                "{\n" +
                "?craft foaf:name \"Apollo 7\" .\n" +
                "?craft foaf:homepage <http://kbss.felk.cvut.cz>\n" +
                "}";
        assertEquals(expected, holder.assembleQuery());
    }

    @Test
    public void assembleQueryWithLiteral() throws Exception {
        holder.setParameter("homepage", new StringParameterValue("http://kbss.felk.cvut.cz", null));
        final String expected = "PREFIX foaf: <http://xmlns.com/foaf/0.1/>\n" +
                "SELECT ?craft\n" +
                "{\n" +
                "?craft foaf:name \"Apollo 7\" .\n" +
                "?craft foaf:homepage \"http://kbss.felk.cvut.cz\"\n" +
                "}";
        assertEquals(expected, holder.assembleQuery());
    }

    @Test
    public void setParametersAndAssembleQueryWithMultipleParamsNextToEachOther() throws Exception {
        final String query = "SELECT ?y ?z WHERE { <http://krizik.felk.cvut.cz/ontologies/jopa#entityA> ?y ?z . }";
        final List<String> params = Arrays.asList("y", "z", "x", "y", "z");
        final List<String> parts = Arrays.asList("SELECT ", " ", " WHERE { ", " ", " ", " . }");
        this.holder = new SparqlQueryHolder(parts, params);
        holder.setParameter("x", new UriParameterValue(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#entityA")));
        final String result = holder.assembleQuery();
        assertEquals(query, result);
    }
}
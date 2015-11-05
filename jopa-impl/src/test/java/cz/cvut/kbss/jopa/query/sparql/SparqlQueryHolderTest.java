package cz.cvut.kbss.jopa.query.sparql;

import cz.cvut.kbss.jopa.model.query.Parameter;
import cz.cvut.kbss.jopa.query.QueryParameter;
import cz.cvut.kbss.jopa.query.parameter.ParameterValue;
import org.junit.Before;
import org.junit.Test;

import java.net.URI;
import java.util.*;
import java.util.stream.Collectors;

import static org.junit.Assert.*;
// TODO
public class SparqlQueryHolderTest {

    private static final List<String> PARTS = Arrays.asList("PREFIX foaf: <http://xmlns.com/foaf/0.1/>\n" +
            "SELECT ", "\n{\n", " foaf:name \"Apollo 7\" .\n", " foaf:homepage ", "\n}");
    private static final List<String> PARAMS = Arrays.asList("craft", "craft", "craft", "homepage");
    private static final Set<String> PARAM_NAMES = new HashSet<>(Arrays.asList("craft", "homepage"));

    private SparqlQueryHolder holder;

    @Before
    public void setUp() throws Exception {
        final List<QueryParameter<?>> parameters = PARAMS.stream().map(QueryParameter::new).collect(
                Collectors.toList());
        this.holder = new SparqlQueryHolder(PARTS, parameters);
    }

    @Test
    public void testGetParameters() throws Exception {
        final Collection<Parameter<?>> res = holder.getParameters();
        assertEquals(PARAM_NAMES.size(), res.size());
        for (Parameter<?> p : res) {
            assertTrue(PARAM_NAMES.contains(p.getName()));
        }
    }

    @Test
    public void testGetParameterByName() throws Exception {
        final String name = PARAMS.get(0);
        final Parameter<?> p = holder.getParameter(name);
        assertNotNull(p);
        assertEquals(name, p.getName());
    }

    @Test
    public void testGetParameterValue() throws Exception {
        final String value = "http://kbss.felk.cvut.cz";
        final Parameter<?> p = new QueryParameter<>("homepage");
        holder.setParameter(p, ParameterValue.create(value));
        assertEquals(value, holder.getParameterValue(p));
    }

    @Test(expected = IllegalArgumentException.class)
    public void getValueOfUnknownParameterThrowsIllegalArgumentException() throws Exception {
        final Parameter<Object> p = new QueryParameter<>("homepage");
        holder.setParameter(p, ParameterValue.create(1));
        holder.getParameterValue(new QueryParameter<>("blabla"));
    }

    @Test
    public void testSetParameter() throws Exception {
        final String value = "http://kbss.felk.cvut.cz";
        final String expected = "\"" + value + "\"";
        holder.setParameter(new QueryParameter<>("homepage"), ParameterValue.create(value));
        assertEquals(expected, holder.getParameterValue(holder.getParameter("homepage")));
    }

    @Test(expected = NullPointerException.class)
    public void setParameterToNullThrowsException() throws Exception {
        holder.setParameter(new QueryParameter<>("homepage"), null);
    }

    @Test(expected = NullPointerException.class)
    public void setNullParameterThrowsException() throws Exception {
        holder.setParameter(null, ParameterValue.create("Whatever"));
    }

    @Test(expected = IllegalArgumentException.class)
    public void setUnknownParameterThrowsException() throws Exception {
        holder.setParameter(new QueryParameter<>("unknown"), ParameterValue.create("Whatever"));
    }

    @Test
    public void clearParameterRemovesParameterValue() throws Exception {
        final QueryParameter<?> qp = new QueryParameter<>("homepage");
        holder.setParameter(qp, ParameterValue.create(URI.create("http://kbss.felk.cvut.cz")));
        assertNotNull(holder.getParameterValue(qp));
        holder.clearParameter(qp);
        assertNull(holder.getParameterValue(qp));
    }

    @Test
    public void clearParametersRemovesAllParameterValues() throws Exception {
        final QueryParameter<?> qp = new QueryParameter<>("homepage");
        holder.setParameter(qp, ParameterValue.create(URI.create("http://kbss.felk.cvut.cz")));
        holder.getParameters().forEach(param -> assertNotNull(holder.getParameterValue(param)));
        holder.clearParameters();
        holder.getParameters().forEach(param -> assertNull(holder.getParameterValue(param)));
    }

    @Test
    public void assembleQueryWithUri() throws Exception {
        final QueryParameter<?> qp = new QueryParameter<>("homepage");
        holder.setParameter(qp, ParameterValue.create(URI.create("http://kbss.felk.cvut.cz")));
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
        final QueryParameter<?> qp = new QueryParameter<>("homepage");
        holder.setParameter(qp, ParameterValue.create("http://kbss.felk.cvut.cz", null));
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
        final List<QueryParameter<?>> params = Arrays
                .asList(new QueryParameter<>("y"), new QueryParameter<>("z"), new QueryParameter<>("x"),
                        new QueryParameter<>("y"), new QueryParameter<>("z"));
        final List<String> parts = Arrays.asList("SELECT ", " ", " WHERE { ", " ", " ", " . }");
        this.holder = new SparqlQueryHolder(parts, params);
        holder.setParameter(new QueryParameter<>("x"), ParameterValue.create(
                URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#entityA")));
        final String result = holder.assembleQuery();
        assertEquals(query, result);
    }
}
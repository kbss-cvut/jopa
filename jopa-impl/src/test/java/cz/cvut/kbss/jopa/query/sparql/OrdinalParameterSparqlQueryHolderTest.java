package cz.cvut.kbss.jopa.query.sparql;

import cz.cvut.kbss.jopa.model.query.Parameter;
import cz.cvut.kbss.jopa.query.QueryParameter;
import org.junit.Before;
import org.junit.Test;

import java.util.*;
import java.util.stream.Collectors;

import static org.junit.Assert.assertEquals;

public class OrdinalParameterSparqlQueryHolderTest {

    private static final String QUERY = "PREFIX foaf: <http://xmlns.com/foaf/0.1/>\n" +
            "SELECT ?craft\n" +
            "{\n" +
            "?craft foaf:name \"Apollo 7\" .\n" +
            "?craft foaf:homepage $1 .\n" +
            "?craft foaf:fundedBy $ .\n" +
            "}";
    private static final List<String> PARTS = Arrays.asList("PREFIX foaf: <http://xmlns.com/foaf/0.1/>\n" +
            "SELECT ", "\n{\n", " foaf:name \"Apollo 7\" .\n", " foaf:homepage ", " .\n", " foaf:fundedBy ", " .\n}");
    private static final List<Object> PARAMS = Arrays.asList("craft", "craft", "craft", 1, "craft", 2);
    private static final Set<Object> PARAM_IDENTIFIERS = new HashSet<>(Arrays.asList("craft", 1, 2));

    private SparqlQueryHolder holder;

    @Before
    public void setUp() throws Exception {
        final Map<Object, QueryParameter<?>> paramsByName = new HashMap<>();
        for (Object n : PARAM_IDENTIFIERS) {
            if (n instanceof String) {
                paramsByName.put(n, new QueryParameter<>((String) n));
            } else {
                paramsByName.put(n, new QueryParameter<>((Integer) n));
            }
        }
        final List<QueryParameter<?>> parameters = PARAMS.stream().map(paramsByName::get).collect(Collectors.toList());
        this.holder = new SparqlQueryHolder(QUERY, PARTS, parameters);
    }

    @Test
    public void testGetOrdinalParameter() {
        final int position = 1;
        final Parameter<?> param = holder.getParameter(position);
        assertEquals(position, param.getPosition().intValue());
    }

    @Test(expected = IllegalArgumentException.class)
    public void getParameterWithUnknownIndexThrowsException() {
        holder.getParameter(Integer.MAX_VALUE);
    }

    @Test
    public void testSetOrdinalParameterValue() {
        final String value = "NASA";
        final QueryParameter<?> param = new QueryParameter<>(2);
        holder.setParameter(param, value);
        assertEquals(value, holder.getParameterValue(param));
    }

    @Test(expected = IllegalArgumentException.class)
    public void setValueOfUnknownParameterThrowsException() {
        holder.setParameter(new QueryParameter<>(Integer.MAX_VALUE), "value");
    }

    @Test
    public void testSetAllParametersAndAssembleQueryWithPositionalParameters() {
        final String fundedBy = "NASA";
        final String homepage = "http://www.apollo-7.com";
        final String query = "PREFIX foaf: <http://xmlns.com/foaf/0.1/>\n" +
                "SELECT ?craft\n" +
                "{\n" +
                "?craft foaf:name \"Apollo 7\" .\n" +
                "?craft foaf:homepage \"" + homepage + "\" .\n" +
                "?craft foaf:fundedBy \"" + fundedBy + "\" .\n" +
                "}";
        holder.setParameter(new QueryParameter<>(1), homepage);
        holder.setParameter(new QueryParameter<>(2), fundedBy);
        assertEquals(query, holder.assembleQuery());
    }

    @Test
    public void testSetSomeParametersAndAssembleQueryWithPositionalParameters() {
        final String fundedBy = "NASA";
        final String query = "PREFIX foaf: <http://xmlns.com/foaf/0.1/>\n" +
                "SELECT ?craft\n" +
                "{\n" +
                "?craft foaf:name \"Apollo 7\" .\n" +
                "?craft foaf:homepage $1 .\n" +
                "?craft foaf:fundedBy \"" + fundedBy + "\" .\n" +
                "}";
        holder.setParameter(new QueryParameter<>(2), fundedBy);
        assertEquals(query, holder.assembleQuery());
    }
}

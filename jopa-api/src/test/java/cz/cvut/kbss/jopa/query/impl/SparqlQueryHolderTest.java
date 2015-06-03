package cz.cvut.kbss.jopa.query.impl;

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

    private static final String QUERY = "PREFIX foaf: <http://xmlns.com/foaf/0.1/>\n" +
            "SELECT ?craft\n" +
            "{\n" +
            "  ?craft foaf:name \"Apollo 7\" .\n" +
            "  ?craft foaf:homepage ?homepage\n" +
            "}";
    private static final List<String> PARTS = Arrays.asList("PREFIX foaf: <http://xmlns.com/foaf/0.1/>\n" +
            "SELECT ", "\n{\n", " foaf:name \"Apollo 7\" .\n  ", " foaf:homepage ", "\n}");
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
    public void testSetParameter() throws Exception {
        final URI value = URI.create("http://kbss.felk.cvut.cz");
        holder.setParameter("homepage", value);
        assertEquals(value, getParameter("homepage"));
    }

    private Object getParameter(String name) throws Exception {
        final Field valuesField = SparqlQueryHolder.class.getDeclaredField("values");
        valuesField.setAccessible(true);
        final Map<?, ?> values = (Map<?, ?>) valuesField.get(holder);
        return values.get(name);
    }

    @Test(expected = NullPointerException.class)
    public void setParameterToNullThrowsException() throws Exception {
        holder.setParameter("homepage", null);
    }

    @Test(expected = NullPointerException.class)
    public void setNullParameterThrowsException() throws Exception {
        holder.setParameter(null, "Whatever");
    }

    @Test(expected = IllegalArgumentException.class)
    public void setUnknownParameterThrowsException() throws Exception {
        holder.setParameter("unknownParameter", "Whatever");
    }
}
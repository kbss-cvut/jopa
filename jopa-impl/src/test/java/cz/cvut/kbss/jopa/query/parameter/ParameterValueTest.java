package cz.cvut.kbss.jopa.query.parameter;

import org.junit.Test;

import java.net.URI;
import java.net.URL;
import java.util.Date;

import static org.junit.Assert.*;

/**
 * Datatype specifications in the assertions are taken from the SPARQL specification.
 */
public class ParameterValueTest {

    @Test
    public void createStringValueWithoutLanguageTag() {
        final ParameterValue value = ParameterValue.create("test");
        assertEquals("test", value.getValue());
        assertEquals("\"test\"", value.getQueryString());
    }

    @Test
    public void createStringValueWithLanguageTag() {
        final ParameterValue value = ParameterValue.create("test", "en");
        assertEquals("test", value.getValue());
        assertEquals("\"test\"@en", value.getQueryString());
    }

    @Test
    public void createBooleanValue() {
        final ParameterValue value = ParameterValue.create(true);
        assertEquals(Boolean.TRUE, value.getValue());
        assertEquals("\"true\"^^<http://www.w3.org/2001/XMLSchema#boolean>", value.getQueryString());
    }

    @Test
    public void createShortValue() {
        final ParameterValue value = ParameterValue.create((short) 117);
        assertEquals((short) 117, value.getValue());
        assertEquals("\"117\"^^<http://www.w3.org/2001/XMLSchema#short>", value.getQueryString());
    }

    @Test
    public void createIntegerValue() {
        final ParameterValue value = ParameterValue.create(117);
        assertEquals(117, value.getValue());
        assertEquals("\"117\"^^<http://www.w3.org/2001/XMLSchema#integer>", value.getQueryString());
    }

    @Test
    public void createLongValue() {
        final long v = System.currentTimeMillis();
        final ParameterValue value = ParameterValue.create(v);
        assertEquals(v, value.getValue());
        assertEquals("\"" + v + "\"^^<http://www.w3.org/2001/XMLSchema#long>", value.getQueryString());
    }

    @Test
    public void createFloatValue() {
        final ParameterValue value = ParameterValue.create(3.14f);
        assertEquals(3.14f, value.getValue());
        assertEquals("\"3.14\"^^<http://www.w3.org/2001/XMLSchema#float>", value.getQueryString());
    }

    @Test
    public void createDoubleValue() {
        final ParameterValue value = ParameterValue.create(3.14);
        assertEquals(3.14, value.getValue());
        assertEquals("\"3.14\"^^<http://www.w3.org/2001/XMLSchema#double>", value.getQueryString());
    }

    @Test
    public void createDateValue() {
        final Date date = new Date();
        final ParameterValue value = ParameterValue.create(date);
        assertEquals(date, value.getValue());
        assertEquals("\"" + date.toString() + "\"^^<http://www.w3.org/2001/XMLSchema#dateTime>",
                value.getQueryString());
    }

    @Test
    public void createUriValue() {
        final URI uri = URI.create("http://krizik.felk.cvut.cz/jopa#Individual");
        final ParameterValue value = ParameterValue.create(uri);
        assertEquals(uri, value.getValue());
        assertEquals("<" + uri.toString() + ">", value.getQueryString());
    }

    @Test
    public void createUrlValue() throws Exception {
        final URL url = new URL("http://krizik.felk.cvut.cz/jopa#Individual");
        final ParameterValue value = ParameterValue.create(url);
        assertEquals(url.toURI(), value.getValue());    // URLs are internally transformed to URIs
        assertEquals("<" + url.toString() + ">", value.getQueryString());
    }
}
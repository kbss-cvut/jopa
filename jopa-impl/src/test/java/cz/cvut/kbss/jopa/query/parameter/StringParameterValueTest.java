package cz.cvut.kbss.jopa.query.parameter;

import org.junit.Test;

import static org.junit.Assert.*;

public class StringParameterValueTest {

    @Test
    public void getQueryStringEscapesNewlinesInQueryParameter() {
        final String value = "fdjsakfsa;f\n        fdjsajsfd";
        final String expected = "\"fdjsakfsa;f\\n        fdjsajsfd\"";
        final StringParameterValue sut = new StringParameterValue(value);
        assertEquals(expected, sut.getQueryString());
    }

    @Test
    public void getQueryStringEscapesEscapeSequencesInValue() {
        final String value = "aaa\nbbb\tccc\bdddd\reee\feee\'fff\'ggg\"hhh\"iii\\jjj";
        final String expected = "\"aaa\\nbbb\\tccc\\bdddd\\reee\\feee\\'fff\\'ggg\\\"hhh\\\"iii\\\\jjj\"";

        final StringParameterValue sut = new StringParameterValue(value);
        assertEquals(expected, sut.getQueryString());
    }
}
package cz.cvut.kbss.jopa.query.soql;

/**
 * SOQL parsing and processing utilities.
 */
public class SoqlUtils {

    private SoqlUtils() {
        throw new AssertionError();
    }

    static String soqlVariableToSparqlVariable(String variable) {
        assert variable.length() >= 2 && (variable.charAt(0) == ':' || variable.charAt(0) == '?');
        return "?" + variable.substring(1);
    }
}

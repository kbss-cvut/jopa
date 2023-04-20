package cz.cvut.kbss.jopa.query.soql;

import cz.cvut.kbss.jopa.exception.QueryParserException;

import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

/**
 * Translates SOQL functions to SPARQL functions.
 */
class SoqlFunctionTranslator {

    private static final Map<String, String> FUNCTION_MAP = initFunctions();

    private static Map<String, String> initFunctions() {
        final Map<String, String> map = new HashMap<>();
        map.put("UPPER", "UCASE");
        map.put("LOWER", "LCASE");
        return map;
    }

    private SoqlFunctionTranslator() {
        throw new AssertionError();
    }

    /**
     * Gets a SPARQL function equivalent to the specified SOQL function.
     *
     * @param soqlFunction SOQL function name
     * @return Matching SPARQL function name
     * @throws QueryParserException If the specified function has no SPARQL equivalent here
     */
    static String getSparqlFunction(String soqlFunction) {
        final String fName = soqlFunction.toUpperCase(Locale.ROOT);
        if (!FUNCTION_MAP.containsKey(fName)) {
            throw new QueryParserException("Unsupported SOQL function '" + soqlFunction + "'.");
        }
        return FUNCTION_MAP.get(fName);
    }
}

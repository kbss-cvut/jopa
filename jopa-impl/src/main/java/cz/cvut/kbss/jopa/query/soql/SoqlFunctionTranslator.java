/**
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
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
        map.put(SoqlConstants.Functions.UPPER, "UCASE");
        map.put(SoqlConstants.Functions.LOWER, "LCASE");
        map.put(SoqlConstants.Functions.LENGTH, "STRLEN");
        map.put(SoqlConstants.Functions.ABS, "ABS");
        map.put(SoqlConstants.Functions.CEIL, "CEIL");
        map.put(SoqlConstants.Functions.FLOOR, "FLOOR");
        map.put(SoqlConstants.Functions.LANG, "lang");
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

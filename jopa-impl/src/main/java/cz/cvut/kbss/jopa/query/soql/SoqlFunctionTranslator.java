/*
 * JOPA
 * Copyright (C) 2026 Czech Technical University in Prague
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
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

    private static final Map<String, String> FUNCTION_MAP = initFuctionMap();

    private SoqlFunctionTranslator() {
        throw new AssertionError();
    }

    private static Map<String, String> initFuctionMap() {
        final Map<String, String> result = new HashMap<>();
        result.put(SoqlConstants.Functions.UPPER, "UCASE");
        result.put(SoqlConstants.Functions.LOWER, "LCASE");
        result.put(SoqlConstants.Functions.LENGTH, "STRLEN");
        result.put(SoqlConstants.Functions.ABS, "ABS");
        result.put(SoqlConstants.Functions.ROUND, "ROUND");
        result.put(SoqlConstants.Functions.CEIL, "CEIL");
        result.put(SoqlConstants.Functions.FLOOR, "FLOOR");
        result.put(SoqlConstants.Functions.CONCAT, "CONCAT");
        result.put(SoqlConstants.Functions.LANG, "lang");
        result.put(SoqlConstants.Functions.LANG_MATCHES, "langMatches");
        result.put(SoqlConstants.Functions.REGEX, "REGEX");
        return result;
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

/*
 * JOPA
 * Copyright (C) 2025 Czech Technical University in Prague
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

import java.util.Locale;
import java.util.Map;

/**
 * Translates SOQL functions to SPARQL functions.
 */
class SoqlFunctionTranslator {

    private static final Map<String, String> FUNCTION_MAP = Map.of(
            SoqlConstants.Functions.UPPER, "UCASE",
            SoqlConstants.Functions.LOWER, "LCASE",
            SoqlConstants.Functions.LENGTH, "STRLEN",
            SoqlConstants.Functions.ABS, "ABS",
            SoqlConstants.Functions.CEIL, "CEIL",
            SoqlConstants.Functions.FLOOR, "FLOOR",
            SoqlConstants.Functions.CONCAT, "concat",
            SoqlConstants.Functions.LANG, "lang",
            SoqlConstants.Functions.LANG_MATCHES, "langMatches"
    );

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

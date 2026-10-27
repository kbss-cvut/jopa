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

import cz.cvut.kbss.jopa.query.parameter.LikePatternTranslator;

/**
 * SOQL ({@code NOT}) {@code LIKE} operator.
 */
class LikeOperator implements FilterableExpression {

    private final boolean isNot;

    private LikeOperator(boolean isNot) {
        this.isNot = isNot;
    }

    @Override
    public String toFilterExpression(String parameter, String value) {
        return (isNot ? "!" : "") + "REGEX(" + parameter + ", " + toRegexPattern(value) + ")";
    }

    private static String toRegexPattern(String value) {
        if (value.length() < 2 || (value.charAt(0) != '\'' && value.charAt(0) != '"')) {
            // Not a string literal (e.g. a variable), leave it for evaluation at runtime
            return value;
        }
        // The pattern is emitted directly into the SPARQL query source text (not through a ParameterValue),
        // so the regex escape backslash has to be escaped itself to remain valid in the SPARQL string literal
        return LikePatternTranslator.translate(value).replace("\\", "\\\\");
    }

    static LikeOperator like() {
        return new LikeOperator(false);
    }

    static LikeOperator notLike() {
        return new LikeOperator(true);
    }
}

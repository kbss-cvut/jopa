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
        char current, previous = ' ';
        final StringBuilder sb = new StringBuilder(value.length());
        for (int i = 0; i < value.length(); i++) {
            current = value.charAt(i);
            if (current == '%' && previous != '\\') {
                sb.append(".*");
            } else if (current == '_' && previous != '\\') {
                sb.append('.');
            } else {
                sb.append(current);
            }
            previous = current;
        }
        return sb.toString();
    }

    static LikeOperator like() {
        return new LikeOperator(false);
    }

    static LikeOperator notLike() {
        return new LikeOperator(true);
    }
}

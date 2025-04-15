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

/**
 * Comparison operator, e.g., &lt;, &gt;.
 */
class ComparisonOperator implements FilterableExpression {

    private final String operator;

    public ComparisonOperator(String operator) {
        assert operator != null;
        this.operator = operator;
    }

    @Override
    public String toFilterExpression(String parameter, String value) {
        return parameter + " " + translateOperator() + " " + value;
    }

    private String translateOperator() {
        return "<>".equals(operator) ? "!=" : operator;
    }

    @Override
    public boolean requiresFilterExpression() {
        return !"=".equals(operator);
    }
}

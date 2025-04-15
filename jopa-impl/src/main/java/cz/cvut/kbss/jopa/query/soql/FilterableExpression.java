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
 * A SOQL expression that may require a SPARQL FILTER expression.
 *
 * Note that currently the expression works for two arguments only.
 */
interface FilterableExpression {

    /**
     * Generates a SPARQL filter expression for the specified first and second argument.
     *
     * @param parameter Variable already declared in the query
     * @param value     Value (variable) used to filter the query-bound variable values
     * @return SPARQL FILTER expression
     */
    String toFilterExpression(String parameter, String value);

    /**
     * Whether this SOQL expression requires a SPARQL FILTER.
     * <p>
     * For example, equality operator does not require a FILTER because a simple triple pattern is functionally
     * equivalent and has to be used anyway.
     *
     * @return Boolean specifying whether this operator is required
     */
    default boolean requiresFilterExpression() {
        return true;
    }
}

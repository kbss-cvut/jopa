/*
 * JOPA
 * Copyright (C) 2024 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.model.query.criteria;

import java.util.List;

public interface Predicate extends Expression<Boolean> {

    /**
     * Return the top-level conjuncts or disjuncts of the predicate. Returns empty list if there are no top-level
     * conjuncts or disjuncts of the predicate.
     *
     * @return list of boolean expressions forming the predicate
     */
    List<Expression<Boolean>> getExpressions();

    /**
     * Return the boolean operator for the predicate. If the predicate is simple, this is AND.
     *
     * @return boolean operator for the predicate
     */
    Predicate.BooleanOperator getOperator();

    /**
     * Create a negation of the predicate.
     *
     * @return negated predicate
     */
    Predicate not();

    /**
     * Determines if the predicate has been created from another predicate by applying the Predicate.not() method.
     *
     * @return boolean indicating if the predicate is a negated predicate
     */
    boolean isNegated();

    enum BooleanOperator {
        AND("AND"),
        OR("OR");

        private final String operator;

        BooleanOperator(String operator) {
            this.operator = operator;
        }

        @Override
        public String toString() {
            return operator;
        }
    }
}

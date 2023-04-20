/**
 * Copyright (C) 2022 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.model.query.criteria.CriteriaQuery;
import cz.cvut.kbss.jopa.model.query.criteria.Expression;
import cz.cvut.kbss.jopa.model.query.criteria.Order;
import cz.cvut.kbss.jopa.model.query.criteria.ParameterExpression;

/**
 * Used to construct criteria queries, compound selections, expressions, predicates, orderings.
 */
public interface CriteriaBuilder extends PredicateFactory {

    /**
     * Create a CriteriaQuery object with the specified result type.
     *
     * @param resultClass type of the query result
     * @return criteria query object
     */
    <T> CriteriaQuery<T> createQuery(Class<T> resultClass);

    /**
     * Create an aggregate expression applying the count operation. Return type of count function in SPARQL is
     * xsd:integer which JOPA internally represents as Integer.
     *
     * @param x expression representing input value to count operation
     * @return count expression
     */
    Expression<Integer> count(Expression<?> x);

    /**
     * Create a parameter expression.
     *
     * @param paramClass parameter class
     * @return parameter expression
     */
    <T> ParameterExpression<T> parameter(Class<T> paramClass);

    /**
     * Create a parameter expression with the given name.
     *
     * @param paramClass parameter class
     * @param name       name that can be used to refer to the parameter
     * @return parameter expression
     */
    <T> ParameterExpression<T> parameter(Class<T> paramClass, String name);

    /**
     * Create an expression for a literal.
     *
     * @param value value represented by the expression
     * @return expression literal
     */
    <T> Expression<T> literal(T value);

    /**
     * Create an expression for a string literal with language tag.
     *
     * @param value       string value represented by the expression
     * @param languageTag string language tag
     * @return expression literal
     */
    Expression<String> literal(String value, String languageTag);

    /**
     * Create expression for converting a string to lowercase.
     *
     * @param x string expression
     * @return expression to convert to lowercase
     */
    Expression<String> lower(Expression<?> x);

    /**
     * Create expression for converting a string to uppercase.
     *
     * @param x string expression
     * @return expression to convert to uppercase
     */
    Expression<String> upper(Expression<?> x);

    /**
     * Create an ordering by the ascending value of the expression.
     *
     * @param x expression used to define the ordering
     * @return ascending ordering corresponding to the expression
     */
    Order asc(Expression<?> x);

    /**
     * Create an ordering by the descending value of the expression.
     *
     * @param x expression used to define the ordering
     * @return descending ordering corresponding to the expression
     */
    Order desc(Expression<?> x);
}

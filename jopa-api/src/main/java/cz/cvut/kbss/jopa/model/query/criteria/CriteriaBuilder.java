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
package cz.cvut.kbss.jopa.model.query.criteria;

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
     * Create an expression that returns the absolute value of its argument.
     *
     * @param x expression
     * @return absolute value
     */
    <N extends Number> Expression<N> abs(Expression<N> x);

    /**
     * Create an expression that returns the smallest (closest to negative infinity) numeric value that is greater than
     * or equal to the argument and is equal to a mathematical integer.
     *
     * @param x expression
     * @return ceiling value
     */
    <N extends Number> Expression<N> ceil(Expression<N> x);

    /**
     * Create an expression that returns the largest (closest to positive infinity) numeric value that is less than or
     * equal to the argument and is equal to a mathematical integer.
     *
     * @param x expression
     * @return floor value
     */
    <N extends Number> Expression<N> floor(Expression<N> x);

    /**
     * Create an aggregate expression applying the count operation. Return type of count function in SPARQL is
     * xsd:integer which JOPA internally represents as Integer.
     *
     * @param x expression representing input value to count operation
     * @return count expression
     */
    Expression<Integer> count(Expression<?> x);

    /**
     * Create expression to return length of a string.
     *
     * @param x string expression
     * @return length expression
     */
    Expression<Integer> length(Expression<String> x);

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
    Expression<String> lower(Expression<String> x);

    /**
     * Create expression for converting a string to uppercase.
     *
     * @param x string expression
     * @return expression to convert to uppercase
     */
    Expression<String> upper(Expression<String> x);

    /**
     * Create expression for extracting language tag from a string literal.
     *
     * @param x String-valued attribute
     * @return Expression to extract language tag
     */
    Expression<String> lang(Path<String> x);

    /**
     * Creates an expression for checking if the specified value matches the specified language range.
     *
     * @param value Language tag value extracting expression
     * @param range Language range expression
     * @return Function call expression
     */
    Expression<Boolean> langMatches(Expression<String> value, Expression<String> range);

    /**
     * Creates an expression for checking if the specified value matches the specified language range.
     *
     * @param value Language tag value extracting expression
     * @param range Language range literal
     * @return Function call expression
     */
    Expression<Boolean> langMatches(Expression<String> value, String range);

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

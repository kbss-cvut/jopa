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

import java.util.Collection;

public interface PredicateFactory {

    /**
     * Create a conjunction of the given boolean expressions.
     * @param x boolean expression
     * @param y boolean expression
     * @return and predicate
     */
    Predicate and(Expression<Boolean> x, Expression<Boolean> y);

    /**
     * Create a conjunction of the given restriction predicates. A conjunction of zero predicates is true.
     * @param restrictions zero or more restriction predicates
     * @return and predicate
     */
    Predicate and(Predicate... restrictions);

    /**
     * Create a disjunction of the given boolean expressions.
     * @param x boolean expression
     * @param y boolean expression
     * @return or predicate
     */
    Predicate or(Expression<Boolean> x, Expression<Boolean> y);

    /**
     * Create a disjunction of the given restriction predicates. A disjunction of zero predicates is false.
     * @param restrictions zero or more restriction predicates
     * @return or predicate
     */
    Predicate or(Predicate... restrictions);

    /**
     * Create a predicate for testing the arguments for equality.
     * @param x expression
     * @param y expression
     * @return equality predicate
     */
    Predicate equal(Expression<?> x, Expression<?> y);

    /**
     * Create a predicate for testing the arguments for equality.
     * @param x expression
     * @param y object
     * @return equality predicate
     */
    Predicate equal(Expression<?> x, Object y);

    /**
     * Create a predicate for testing the arguments for equality.
     * @param x expression
     * @param y string
     * @param languageTag string
     * @return equality predicate
     */
    Predicate equal(Expression<?> x, String y, String languageTag);

    /**
     * Create a predicate for testing the arguments for inequality.
     * @param x expression
     * @param y expression
     * @return inequality predicate
     */
    Predicate notEqual(Expression<?> x, Expression<?> y);

    /**
     * Create a predicate for testing the arguments for inequality.
     * @param x expression
     * @param y object
     * @return inequality predicate
     */
    Predicate notEqual(Expression<?> x, Object y);

    /**
     * Create a predicate for testing whether the first argument is greater than the second.
     * @param x expression
     * @param y expression
     * @return greaterThan predicate
     */
    <Y extends Comparable<? super Y>> Predicate greaterThan(Expression<? extends Y> x, Expression<? extends Y> y);

    /**
     * Create a predicate for testing whether the first argument is greater than the second.
     * @param x expression
     * @param y value
     * @return greaterThan predicate
     */
    <Y extends Comparable<? super Y>> Predicate greaterThan(Expression<? extends Y> x, Y y);

    /**
     * Create a predicate for testing whether the first argument is greater than or equal to the second.
     * @param x expression
     * @param y expression
     * @return greaterThanOrEqual predicate
     */
    <Y extends Comparable<? super Y>> Predicate greaterThanOrEqual(Expression<? extends Y> x,
                                                                   Expression<? extends Y> y);

    /**
     * Create a predicate for testing whether the first argument is greater than or equal to the second.
     * @param x expression
     * @param y value
     * @return greaterThanOrEqual predicate
     */
    <Y extends Comparable<? super Y>> Predicate greaterThanOrEqual(Expression<? extends Y> x, Y y);

    /**
     * Create a predicate for testing whether the first argument is less than the second.
     * @param x expression
     * @param y expression
     * @return lessThan predicate
     */
    <Y extends Comparable<? super Y>> Predicate lessThan(Expression<? extends Y> x, Expression<? extends Y> y);

    /**
     * Create a predicate for testing whether the first argument is less than the second.
     * @param x expression
     * @param y value
     * @return lessThan predicate
     */
    <Y extends Comparable<? super Y>> Predicate lessThan(Expression<? extends Y> x, Y y);

    /**
     * Create a predicate for testing whether the first argument is less than or equal to the second.
     * @param x expression
     * @param y expression
     * @return lessThanOrEqual predicate
     */
    <Y extends Comparable<? super Y>> Predicate lessThanOrEqual(Expression<? extends Y> x, Expression<? extends Y> y);

    /**
     * Create a predicate for testing whether the first argument is less than or equal to the second.
     * @param x expression
     * @param y value
     * @return lessThanOrEqual predicate
     */
    <Y extends Comparable<? super Y>> Predicate lessThanOrEqual(Expression<? extends Y> x, Y y);

    /**
     * Create a predicate for testing whether the expression satisfies the given pattern.
     * @param x string expression
     * @param pattern string expression
     * @return like predicate
     */
    Predicate like(Expression<String> x, Expression<String> pattern);

    /**
     * Create a predicate for testing whether the expression satisfies the given pattern.
     * @param x string expression
     * @param pattern string
     * @return like predicate
     */
    Predicate like(Expression<String> x, String pattern);

    /**
     * Create a predicate for testing whether the expression does not satisfy the given pattern.
     * @param x string expression
     * @param pattern string expression
     * @return like predicate
     */
    Predicate notLike(Expression<String> x, Expression<String> pattern);

    /**
     * Create a predicate for testing whether the expression does not satisfy the given pattern.
     * @param x string expression
     * @param pattern string
     * @return like predicate
     */
    Predicate notLike(Expression<String> x, String pattern);

    /**
     * Creates a predicate that tests whether an element is a member of a collection.
     *
     * If the collection is empty, the predicate will be false.
     * @param elem Element
     * @param collection Expression
     * @return is-member predicate
     */
    <E, C extends Collection<E>> Predicate isMember(E elem, Expression<C> collection);

    /**
     * Creates a predicate that tests whether an element is not a member of a collection.
     *
     * If the collection is empty, the predicate will be true.
     * @param elem Element
     * @param collection Expression
     * @return is-member predicate
     */
    <E, C extends Collection<E>> Predicate isNotMember(E elem, Expression<C> collection);

    /**
     * Create a negation of the given restriction.
     * @param restriction restriction expression
     * @return not predicate
     */
    Predicate not(Expression<Boolean> restriction);

    /**
     * Create predicate to test whether given expression is contained in a list of values.
     * @param expression - to be tested against list of values
     * @return in predicate
     */
    <T> PredicateFactory.In<T> in(Expression<? extends T> expression);

    /**
     * Create predicate to test whether given expression is not contained in a list of values.
     * @param expression  - to be tested against list of values
     * @return not in predicate
     */
    <T> In<T> notIn(Expression<? extends T> expression);

    /**
     * Interface used to build in predicates.
     * @param <T>
     */
    interface In<T> extends Predicate {

        /**
         * Return the expression to be tested against the list of values.
         * @return expression
         */
        Expression<T> getExpression();

        /**
         * Add to list of values to be tested against.
         * @param value - value
         * @return in predicate
         */
        PredicateFactory.In<T> value(T value);
    }
}

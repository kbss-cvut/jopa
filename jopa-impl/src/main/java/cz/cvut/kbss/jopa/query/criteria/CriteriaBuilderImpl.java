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
package cz.cvut.kbss.jopa.query.criteria;

import cz.cvut.kbss.jopa.model.CriteriaQueryImpl;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.model.query.criteria.CriteriaBuilder;
import cz.cvut.kbss.jopa.model.query.criteria.Expression;
import cz.cvut.kbss.jopa.model.query.criteria.Order;
import cz.cvut.kbss.jopa.model.query.criteria.ParameterExpression;
import cz.cvut.kbss.jopa.model.query.criteria.Path;
import cz.cvut.kbss.jopa.model.query.criteria.Predicate;
import cz.cvut.kbss.jopa.query.criteria.expressions.AbsFunction;
import cz.cvut.kbss.jopa.query.criteria.expressions.AbstractExpression;
import cz.cvut.kbss.jopa.query.criteria.expressions.AbstractPathExpression;
import cz.cvut.kbss.jopa.query.criteria.expressions.CeilFunction;
import cz.cvut.kbss.jopa.query.criteria.expressions.CountFunction;
import cz.cvut.kbss.jopa.query.criteria.expressions.ExpressionEqualImpl;
import cz.cvut.kbss.jopa.query.criteria.expressions.ExpressionGreaterThanImpl;
import cz.cvut.kbss.jopa.query.criteria.expressions.ExpressionGreaterThanOrEqualImpl;
import cz.cvut.kbss.jopa.query.criteria.expressions.ExpressionInImpl;
import cz.cvut.kbss.jopa.query.criteria.expressions.ExpressionLessThanImpl;
import cz.cvut.kbss.jopa.query.criteria.expressions.ExpressionLessThanOrEqualImpl;
import cz.cvut.kbss.jopa.query.criteria.expressions.ExpressionLikeImpl;
import cz.cvut.kbss.jopa.query.criteria.expressions.ExpressionLiteralImpl;
import cz.cvut.kbss.jopa.query.criteria.expressions.ExpressionNotEqualImpl;
import cz.cvut.kbss.jopa.query.criteria.expressions.ExpressionNotLikeImpl;
import cz.cvut.kbss.jopa.query.criteria.expressions.FloorFunction;
import cz.cvut.kbss.jopa.query.criteria.expressions.IsMemberExpression;
import cz.cvut.kbss.jopa.query.criteria.expressions.LangFunction;
import cz.cvut.kbss.jopa.query.criteria.expressions.LangMatchesFunction;
import cz.cvut.kbss.jopa.query.criteria.expressions.LengthFunction;
import cz.cvut.kbss.jopa.query.criteria.expressions.LowerFunction;
import cz.cvut.kbss.jopa.query.criteria.expressions.OrderImpl;
import cz.cvut.kbss.jopa.query.criteria.expressions.ParameterExpressionImpl;
import cz.cvut.kbss.jopa.query.criteria.expressions.UpperFunction;

import java.util.Arrays;
import java.util.Collection;

public class CriteriaBuilderImpl implements CriteriaBuilder {

    private final Metamodel metamodel;

    public CriteriaBuilderImpl(Metamodel metamodel) {
        this.metamodel = metamodel;
    }

    @Override
    public <T> CriteriaQueryImpl<T> createQuery(Class<T> resultClass) {
        return new CriteriaQueryImpl<>(new CriteriaQueryHolder<>(resultClass), metamodel, this);
    }

    @Override
    public <N extends Number> Expression<N> abs(Expression<N> x) {
        validateFunctionArgument(x);
        return new AbsFunction<>((Class<N>) x.getJavaType(), (AbstractPathExpression) x, this);
    }

    private static void validateFunctionArgument(Expression<?> x) {
        if (!(x instanceof AbstractPathExpression)) {
            throw new IllegalArgumentException("Function can be applied only to path expressions.");
        }
    }

    @Override
    public Expression<Integer> count(Expression<?> x) {
        validateFunctionArgument(x);
        return new CountFunction((AbstractPathExpression) x, this);
    }

    @Override
    public <N extends Number> Expression<N> ceil(Expression<N> x) {
        validateFunctionArgument(x);
        return new CeilFunction<>((Class<N>) x.getJavaType(), (AbstractPathExpression) x, this);
    }

    @Override
    public <N extends Number> Expression<N> floor(Expression<N> x) {
        validateFunctionArgument(x);
        return new FloorFunction<>((Class<N>) x.getJavaType(), (AbstractPathExpression) x, this);
    }

    @Override
    public Expression<Integer> length(Expression<String> x) {
        validateFunctionArgument(x);
        return new LengthFunction((AbstractPathExpression) x, this);
    }

    @Override
    public <T> ParameterExpression<T> parameter(Class<T> paramClass) {
        if (paramClass == null) {
            throw new IllegalArgumentException("Class must be defined.");
        }
        return new ParameterExpressionImpl<>(paramClass, null, this);
    }

    @Override
    public <T> ParameterExpression<T> parameter(Class<T> paramClass, String name) {
        if (paramClass == null) {
            throw new IllegalArgumentException("Class must be defined.");
        }
        return new ParameterExpressionImpl<>(paramClass, name, this);
    }

    @Override
    public <T> Expression<T> literal(T value) {
        if (value == null) {
            throw new IllegalArgumentException("Literal cannot be null.");
        }
        return new ExpressionLiteralImpl<>(value, this);
    }

    @Override
    public Expression<String> literal(String value, String languageTag) {
        if (value == null) {
            throw new IllegalArgumentException("Literal cannot be null.");
        }
        return new ExpressionLiteralImpl<>(value, languageTag, this);
    }

    @Override
    public Expression<String> lower(Expression<String> x) {
        validateFunctionArgument(x);
        return new LowerFunction((AbstractPathExpression) x, this);
    }

    @Override
    public Expression<String> upper(Expression<String> x) {
        validateFunctionArgument(x);
        return new UpperFunction((AbstractPathExpression) x, this);
    }

    @Override
    public Expression<String> lang(Path<String> x) {
        validateFunctionArgument(x);
        return new LangFunction((AbstractPathExpression) x, this);
    }

    @Override
    public Expression<Boolean> langMatches(Expression<String> value, Expression<String> range) {
        return new LangMatchesFunction(this, (AbstractExpression<String>) value, (AbstractExpression<String>) range);
    }

    @Override
    public Expression<Boolean> langMatches(Expression<String> value, String range) {
        return langMatches(value, new ExpressionLiteralImpl<>(range, this));
    }

    @Override
    public Order asc(Expression<?> x) {
        return new OrderImpl(x);
    }

    @Override
    public Order desc(Expression<?> x) {
        return new OrderImpl(x, false);
    }


    @Override
    public Predicate and(Expression<Boolean> x, Expression<Boolean> y) {
        return new CompoundedPredicateImpl(Predicate.BooleanOperator.AND, Arrays.asList(x, y), this);
    }

    @Override
    public Predicate and(Predicate... restrictions) {
        if (restrictions.length == 1) {
            return new SimplePredicateImpl(restrictions[0], this);
        } else {
            return new CompoundedPredicateImpl(Predicate.BooleanOperator.AND, Arrays.asList(restrictions), this);
        }
    }

    @Override
    public Predicate or(Expression<Boolean> x, Expression<Boolean> y) {
        return new CompoundedPredicateImpl(Predicate.BooleanOperator.OR, Arrays.asList(x, y), this);
    }

    @Override
    public Predicate or(Predicate... restrictions) {
        if (restrictions.length == 1) {
            return new SimplePredicateImpl(Predicate.BooleanOperator.OR, restrictions[0], this);
        } else {
            return new CompoundedPredicateImpl(Predicate.BooleanOperator.OR, Arrays.asList(restrictions), this);
        }
    }

    @Override
    public Predicate equal(Expression<?> x, Expression<?> y) {
        return new SimplePredicateImpl(
                new ExpressionEqualImpl((AbstractExpression<?>) x, (AbstractExpression<?>) y, this), this);
    }

    @Override
    public Predicate equal(Expression<?> x, Object y) {
        return new SimplePredicateImpl(
                new ExpressionEqualImpl((AbstractExpression<?>) x, new ExpressionLiteralImpl<>(y, this), this), this);
    }

    @Override
    public Predicate equal(Expression<?> x, String y, String languageTag) {
        return new SimplePredicateImpl(
                new ExpressionEqualImpl((AbstractExpression<?>) x, new ExpressionLiteralImpl<>(y, languageTag, this),
                        this), this);
    }

    @Override
    public Predicate notEqual(Expression<?> x, Expression<?> y) {
        return new SimplePredicateImpl(
                new ExpressionNotEqualImpl((AbstractExpression<?>) x, (AbstractExpression<?>) y, this), this);
    }

    @Override
    public Predicate notEqual(Expression<?> x, Object y) {
        return new SimplePredicateImpl(
                new ExpressionNotEqualImpl((AbstractExpression<?>) x, new ExpressionLiteralImpl<>(y, this), this),
                this);

    }

    @Override
    public Predicate like(Expression<String> x, Expression<String> pattern) {
        return new SimplePredicateImpl(
                new ExpressionLikeImpl((AbstractExpression<String>) x, (AbstractExpression<String>) pattern, this),
                this);
    }

    @Override
    public Predicate like(Expression<String> x, String pattern) {
        return new SimplePredicateImpl(
                new ExpressionLikeImpl((AbstractExpression<String>) x, new ExpressionLiteralImpl<>(pattern, this),
                        this), this);
    }

    @Override
    public Predicate notLike(Expression<String> x, Expression<String> pattern) {
        return new SimplePredicateImpl(
                new ExpressionNotLikeImpl((AbstractExpression<String>) x, (AbstractExpression<String>) pattern, this),
                this);
    }

    @Override
    public Predicate notLike(Expression<String> x, String pattern) {
        return new SimplePredicateImpl(
                new ExpressionNotLikeImpl((AbstractExpression<String>) x, new ExpressionLiteralImpl<>(pattern, this),
                        this), this);
    }

    @Override
    public Predicate not(Expression<Boolean> restriction) {
        return wrapExpressionToPredicateWithRepair(restriction).not();
    }

    @Override
    public <T> In<T> in(Expression<? extends T> expression) {
        return new ExpressionInImpl<>(expression, this);
    }

    @Override
    public <T> In<T> notIn(Expression<? extends T> expression) {
        In<T> inExpression = new ExpressionInImpl<>(expression, this);
        inExpression.not();
        return inExpression;
    }

    @Override
    public <E, C extends Collection<E>> Predicate isMember(E elem, Expression<C> collection) {
        return new IsMemberExpression<>(elem, collection, this);
    }

    @Override
    public <E, C extends Collection<E>> Predicate isNotMember(E elem, Expression<C> collection) {
        final IsMemberExpression<E> expr = new IsMemberExpression<>(elem, collection, this);
        expr.not();
        return expr;
    }

    @Override
    public <Y extends Comparable<? super Y>> Predicate greaterThan(Expression<? extends Y> x,
                                                                   Expression<? extends Y> y) {
        return new SimplePredicateImpl(
                new ExpressionGreaterThanImpl((AbstractExpression<Y>) x, (AbstractExpression<Y>) y, this), this);
    }

    @Override
    public <Y extends Comparable<? super Y>> Predicate greaterThan(Expression<? extends Y> x, Y y) {
        return new SimplePredicateImpl(
                new ExpressionGreaterThanImpl((AbstractExpression<Y>) x, new ExpressionLiteralImpl<>(y, this), this),
                this);
    }

    @Override
    public <Y extends Comparable<? super Y>> Predicate greaterThanOrEqual(Expression<? extends Y> x,
                                                                          Expression<? extends Y> y) {
        return new SimplePredicateImpl(
                new ExpressionGreaterThanOrEqualImpl((AbstractExpression<Y>) x, (AbstractExpression<Y>) y, this), this);
    }

    @Override
    public <Y extends Comparable<? super Y>> Predicate greaterThanOrEqual(Expression<? extends Y> x, Y y) {
        return new SimplePredicateImpl(
                new ExpressionGreaterThanOrEqualImpl((AbstractExpression<Y>) x, new ExpressionLiteralImpl<>(y, this),
                        this), this);
    }

    @Override
    public <Y extends Comparable<? super Y>> Predicate lessThan(Expression<? extends Y> x, Expression<? extends Y> y) {
        return new SimplePredicateImpl(
                new ExpressionLessThanImpl((AbstractExpression<Y>) x, (AbstractExpression<Y>) y, this), this);
    }

    @Override
    public <Y extends Comparable<? super Y>> Predicate lessThan(Expression<? extends Y> x, Y y) {
        return new SimplePredicateImpl(
                new ExpressionLessThanImpl((AbstractExpression<Y>) x, new ExpressionLiteralImpl<>(y, this), this),
                this);
    }

    @Override
    public <Y extends Comparable<? super Y>> Predicate lessThanOrEqual(Expression<? extends Y> x,
                                                                       Expression<? extends Y> y) {
        return new SimplePredicateImpl(
                new ExpressionLessThanOrEqualImpl((AbstractExpression<Y>) x, (AbstractExpression<Y>) y, this), this);
    }

    @Override
    public <Y extends Comparable<? super Y>> Predicate lessThanOrEqual(Expression<? extends Y> x, Y y) {
        return new SimplePredicateImpl(
                new ExpressionLessThanOrEqualImpl((AbstractExpression<Y>) x, new ExpressionLiteralImpl<>(y, this),
                        this), this);
    }


    /**
     * Method wraps given boolean expression to Predicate and if path expression occur, it wrap it to
     * ExpressionEqualsImpl before. For example:
     * {@literal Expression<Boolean> expression = factory.get("attributeName");} Looks like boolean expression but in
     * fact it is not boolean expression, so we need to fix this.
     *
     * @param expression - boolean or path expression
     * @return Expression wrapped in Predicate
     */
    public Predicate wrapExpressionToPredicateWithRepair(Expression<Boolean> expression) {
        if (expression instanceof Predicate) {
            return (Predicate) expression;
        } else if (expression instanceof AbstractPathExpression) {
            return new SimplePredicateImpl(
                    new ExpressionEqualImpl((AbstractExpression) expression, (AbstractExpression) this.literal(true),
                            this), this);
        } else {
            return new SimplePredicateImpl(expression, this);
        }
    }

}

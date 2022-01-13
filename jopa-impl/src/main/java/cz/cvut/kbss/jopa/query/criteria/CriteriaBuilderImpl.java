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
package cz.cvut.kbss.jopa.query.criteria;

import cz.cvut.kbss.jopa.model.CriteriaQueryImpl;
import cz.cvut.kbss.jopa.model.query.criteria.*;
import cz.cvut.kbss.jopa.query.criteria.expressions.*;
import cz.cvut.kbss.jopa.sessions.CriteriaBuilder;
import cz.cvut.kbss.jopa.sessions.UnitOfWorkImpl;

import java.util.Arrays;

public class CriteriaBuilderImpl implements CriteriaBuilder {

    private final UnitOfWorkImpl uow;

    public CriteriaBuilderImpl(UnitOfWorkImpl uow) {
        this.uow = uow;
    }

    @Override
    public <T> CriteriaQuery<T> createQuery(Class<T> resultClass) {
        return new CriteriaQueryImpl<>(new CriteriaQueryHolder<>(resultClass), uow.getMetamodel(), this);
    }

    @Override
    public Expression<Integer> count(Expression<?> x) {
        if (x == null) throw new IllegalArgumentException("Aggregate function cannot be applied to null expression.");
        if (x instanceof AbstractPathExpression) {
            return new ExpressionCountImpl<>(Integer.class, (AbstractPathExpression) x, this);
        }
        throw new IllegalArgumentException("Aggregate function can be applied only to path expressions.");
    }


    @Override
    public <T> ParameterExpression<T> parameter(Class<T> paramClass) {
        if (paramClass == null) throw new IllegalArgumentException("Class must be defined.");
        return new ParameterExpressionImpl<>(paramClass, null, this);
    }

    @Override
    public <T> ParameterExpression<T> parameter(Class<T> paramClass, String name) {
        if (paramClass == null) throw new IllegalArgumentException("Class must be defined.");
        return new ParameterExpressionImpl<>(paramClass, name, this);
    }

    @Override
    public <T> Expression<T> literal(T value) throws IllegalArgumentException {
        if (value == null) throw new IllegalArgumentException("Literal cannot be null.");
        return new ExpressionLiteralImpl<>(value, this);
    }

    @Override
    public Expression<String> literal(String value, String languageTag) throws IllegalArgumentException {
        if (value == null) throw new IllegalArgumentException("Literal cannot be null.");
        return new ExpressionLiteralImpl<>(value, languageTag, this);
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
        if (restrictions.length == 1) return new SimplePredicateImpl(restrictions[0], this);
        else return new CompoundedPredicateImpl(Predicate.BooleanOperator.AND, Arrays.asList(restrictions), this);
    }

    @Override
    public Predicate or(Expression<Boolean> x, Expression<Boolean> y) {
        return new CompoundedPredicateImpl(Predicate.BooleanOperator.OR, Arrays.asList(x, y), this);
    }

    @Override
    public Predicate or(Predicate... restrictions) {
        if (restrictions.length == 1)
            return new SimplePredicateImpl(Predicate.BooleanOperator.OR, restrictions[0], this);
        else return new CompoundedPredicateImpl(Predicate.BooleanOperator.OR, Arrays.asList(restrictions), this);
    }

    @Override
    public Predicate equal(Expression<?> x, Expression<?> y) {
        return new SimplePredicateImpl(new ExpressionEqualImpl((AbstractExpression<?>) x, (AbstractExpression<?>) y, this), this);
    }

    @Override
    public Predicate equal(Expression<?> x, Object y) {
        return new SimplePredicateImpl(new ExpressionEqualImpl((AbstractExpression<?>) x, new ExpressionLiteralImpl<>(y, this), this), this);
    }

    @Override
    public Predicate equal(Expression<?> x, String y, String languageTag) {
        return new SimplePredicateImpl(new ExpressionEqualImpl((AbstractExpression<?>) x, new ExpressionLiteralImpl<>(y, languageTag, this), this), this);
    }

    @Override
    public Predicate notEqual(Expression<?> x, Expression<?> y) {
        return new SimplePredicateImpl(new ExpressionNotEqualImpl((AbstractExpression<?>) x, (AbstractExpression<?>) y, this), this);
    }

    @Override
    public Predicate notEqual(Expression<?> x, Object y) {
        return new SimplePredicateImpl(new ExpressionNotEqualImpl((AbstractExpression<?>) x, new ExpressionLiteralImpl<>(y, this), this), this);

    }

    @Override
    public Predicate like(Expression<String> x, Expression<String> pattern) {
        return new SimplePredicateImpl(new ExpressionLikeImpl((AbstractExpression<String>) x, (AbstractExpression<String>) pattern, this), this);
    }

    @Override
    public Predicate like(Expression<String> x, String pattern) {
        return new SimplePredicateImpl(new ExpressionLikeImpl((AbstractExpression<String>) x, new ExpressionLiteralImpl<>(pattern, this), this), this);
    }

    @Override
    public Predicate notLike(Expression<String> x, Expression<String> pattern) {
        return new SimplePredicateImpl(new ExpressionNotLikeImpl((AbstractExpression<String>) x, (AbstractExpression<String>) pattern, this), this);
    }

    @Override
    public Predicate notLike(Expression<String> x, String pattern) {
        return new SimplePredicateImpl(new ExpressionNotLikeImpl((AbstractExpression<String>) x, new ExpressionLiteralImpl<>(pattern, this), this), this);
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
    public <Y extends Comparable<? super Y>> Predicate greaterThan(Expression<? extends Y> x, Expression<? extends Y> y) {
        return new SimplePredicateImpl(new ExpressionGreaterThanImpl((AbstractExpression<Y>) x, (AbstractExpression<Y>) y, this), this);
    }

    @Override
    public <Y extends Comparable<? super Y>> Predicate greaterThan(Expression<? extends Y> x, Y y) {
        return new SimplePredicateImpl(new ExpressionGreaterThanImpl((AbstractExpression<Y>) x, new ExpressionLiteralImpl<>(y, this), this), this);
    }

    @Override
    public <Y extends Comparable<? super Y>> Predicate greaterThanOrEqual(Expression<? extends Y> x, Expression<? extends Y> y) {
        return new SimplePredicateImpl(new ExpressionGreaterThanOrEqualImpl((AbstractExpression<Y>) x, (AbstractExpression<Y>) y, this), this);
    }

    @Override
    public <Y extends Comparable<? super Y>> Predicate greaterThanOrEqual(Expression<? extends Y> x, Y y) {
        return new SimplePredicateImpl(new ExpressionGreaterThanOrEqualImpl((AbstractExpression<Y>) x, new ExpressionLiteralImpl<>(y, this), this), this);
    }

    @Override
    public <Y extends Comparable<? super Y>> Predicate lessThan(Expression<? extends Y> x, Expression<? extends Y> y) {
        return new SimplePredicateImpl(new ExpressionLessThanImpl((AbstractExpression<Y>) x, (AbstractExpression<Y>) y, this), this);
    }

    @Override
    public <Y extends Comparable<? super Y>> Predicate lessThan(Expression<? extends Y> x, Y y) {
        return new SimplePredicateImpl(new ExpressionLessThanImpl((AbstractExpression<Y>) x, new ExpressionLiteralImpl<>(y, this), this), this);
    }

    @Override
    public <Y extends Comparable<? super Y>> Predicate lessThanOrEqual(Expression<? extends Y> x, Expression<? extends Y> y) {
        return new SimplePredicateImpl(new ExpressionLessThanOrEqualImpl((AbstractExpression<Y>) x, (AbstractExpression<Y>) y, this), this);
    }

    @Override
    public <Y extends Comparable<? super Y>> Predicate lessThanOrEqual(Expression<? extends Y> x, Y y) {
        return new SimplePredicateImpl(new ExpressionLessThanOrEqualImpl((AbstractExpression<Y>) x, new ExpressionLiteralImpl<>(y, this), this), this);
    }


    /**
     * Method wraps given boolean expression to Predicate and if path expression occur, it wrap it to ExpressionEqualsImpl before.
     * For example:
     * {@literal Expression<Boolean> expression = factory.get("attributeName");}
     * Looks like boolean expression but in fact it is not boolean expression, so we need to fix this.
     *
     * @param expression - boolean or path expression
     * @return Expression wrapped in Predicate
     */
    public Predicate wrapExpressionToPredicateWithRepair(Expression<Boolean> expression) {
        if (expression instanceof Predicate) {
            return (Predicate) expression;
        } else if (expression instanceof AbstractPathExpression) {
            return new SimplePredicateImpl(new ExpressionEqualImpl((AbstractExpression) expression, (AbstractExpression) this.literal(true), this), this);
        } else {
            return new SimplePredicateImpl(expression, this);
        }
    }

}

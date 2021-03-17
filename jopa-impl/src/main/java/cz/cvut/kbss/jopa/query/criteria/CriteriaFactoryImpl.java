package cz.cvut.kbss.jopa.query.criteria;

import cz.cvut.kbss.jopa.exception.MissingChildExpressionException;
import cz.cvut.kbss.jopa.model.CriteriaQueryImpl;
import cz.cvut.kbss.jopa.model.query.criteria.CriteriaQuery;
import cz.cvut.kbss.jopa.model.query.criteria.Expression;
import cz.cvut.kbss.jopa.model.query.criteria.Predicate;
import cz.cvut.kbss.jopa.query.criteria.expressions.*;
import cz.cvut.kbss.jopa.sessions.CriteriaFactory;
import cz.cvut.kbss.jopa.sessions.UnitOfWorkImpl;

public class CriteriaFactoryImpl implements CriteriaFactory {

    private final UnitOfWorkImpl uow;

    public CriteriaFactoryImpl(UnitOfWorkImpl uow) {
        this.uow = uow;
    }

    //TODO - query without resultClass
    @Override
    public CriteriaQuery<Object> createQuery() {
        return null;
    }

    @Override
    public <T> CriteriaQuery<T> createQuery(Class<T> resultClass) {
        return new CriteriaQueryImpl<>(new CriteriaQueryHolder<>(resultClass), uow.getMetamodel());
    }

    @Override
    public Expression<Long> count(Expression<?> x) {
//        AbstractExpression<?> expression = (AbstractExpression<?>) x;
//        if (expression.getExpression() == null) throw new MissingChildExpressionException("Expression representing COUNT method is missing child expression.");
//        return new FunctionExpressionImpl<>(new ExpressionCountImpl<>(expression.getExpression()));
        return null;
    }

    @Override
    public Predicate and(Expression<Boolean> x, Expression<Boolean> y) {
        return null;
    }

    @Override
    public Predicate and(Predicate... restrictions) {
        return null;
    }

    @Override
    public Predicate or(Expression<Boolean> x, Expression<Boolean> y) {
        return null;
    }

    @Override
    public Predicate or(Predicate... restrictions) {
        return null;
    }

    @Override
    public Predicate equals(Expression<?> x, Expression<?> y) {
//        return new PredicateImpl(new ExpressionEqualsImpl<>((AbstractExpression<?>)x,(AbstractExpression<?>)y));
        return null;
    }

    @Override
    public Predicate equals(Expression<?> x, Object y) {
//        return new PredicateImpl(new ExpressionEqualsImpl<>((AbstractExpression<?>)x, y));
        return null;
    }

    @Override
    public Predicate notEquals(Expression<?> x, Expression<?> y) {
//        return new PredicateImpl(new ExpressionNotEqualsImpl<>((AbstractExpression<?>)x,(AbstractExpression<?>)y));
        return null;
    }

    @Override
    public Predicate notEquals(Expression<?> x, Object y) {
//        return new PredicateImpl(new ExpressionNotEqualsImpl<>((AbstractExpression<?>)x,y));
        return null;
    }

    @Override
    public <Y extends Comparable<? super Y>> Predicate greaterThan(Expression<? extends Y> x, Expression<? extends Y> y) {
//        return new PredicateImpl(new ExpressionGreaterThanImpl<>((AbstractExpression<?>)x,(AbstractExpression<?>)y));
        return null;
    }

    @Override
    public <Y extends Comparable<? super Y>> Predicate greaterThan(Expression<? extends Y> x, Y y) {
//        return new PredicateImpl(new ExpressionGreaterThanImpl<>((AbstractExpression<?>)x,y));
        return null;
    }

    @Override
    public <Y extends Comparable<? super Y>> Predicate greaterOrEqual(Expression<? extends Y> x, Expression<? extends Y> y) {
//        return new PredicateImpl(new ExpressionGreaterThanOrEqualImpl<>((AbstractExpression<?>)x,(AbstractExpression<?>)y));
        return null;
    }

    @Override
    public <Y extends Comparable<? super Y>> Predicate greaterOrEqual(Expression<? extends Y> x, Y y) {
//        return new PredicateImpl(new ExpressionGreaterThanOrEqualImpl<>((AbstractExpression<?>)x,y));
        return null;
    }

    @Override
    public <Y extends Comparable<? super Y>> Predicate lessThan(Expression<? extends Y> x, Expression<? extends Y> y) {
//        return new PredicateImpl(new ExpressionLessThanImpl<>((AbstractExpression<?>)x,(AbstractExpression<?>)y));
        return null;
    }

    @Override
    public <Y extends Comparable<? super Y>> Predicate lessThan(Expression<? extends Y> x, Y y) {
//        return new PredicateImpl(new ExpressionLessThanImpl<>((AbstractExpression<?>)x,y));
        return null;
    }

    @Override
    public <Y extends Comparable<? super Y>> Predicate lessOrEqual(Expression<? extends Y> x, Expression<? extends Y> y) {
//        return new PredicateImpl(new ExpressionLessThanOrEqualImpl<>((AbstractExpression<?>)x,(AbstractExpression<?>)y));
        return null;
    }

    @Override
    public <Y extends Comparable<? super Y>> Predicate lessOrEqual(Expression<? extends Y> x, Y y) {
//        return new PredicateImpl(new ExpressionLessThanOrEqualImpl<>((AbstractExpression<?>)x,y));
        return null;
    }

}

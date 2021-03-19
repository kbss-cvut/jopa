package cz.cvut.kbss.jopa.query.criteria;

import com.sun.javafx.fxml.expression.LiteralExpression;
import cz.cvut.kbss.jopa.model.CriteriaQueryImpl;
import cz.cvut.kbss.jopa.model.query.criteria.CriteriaQuery;
import cz.cvut.kbss.jopa.model.query.criteria.Expression;
import cz.cvut.kbss.jopa.model.query.criteria.ParameterExpression;
import cz.cvut.kbss.jopa.model.query.criteria.Predicate;
import cz.cvut.kbss.jopa.query.criteria.expressions.*;
import cz.cvut.kbss.jopa.sessions.CriteriaFactory;
import cz.cvut.kbss.jopa.sessions.UnitOfWorkImpl;

import java.util.Arrays;

public class CriteriaFactoryImpl implements CriteriaFactory {

    private final UnitOfWorkImpl uow;

    public CriteriaFactoryImpl(UnitOfWorkImpl uow) {
        this.uow = uow;
    }


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
    public <T> ParameterExpression<T> parameter(Class<T> paramClass) {
        return new ParameterExpressionImpl<>(paramClass, null);
    }

    @Override
    public <T> ParameterExpression<T> parameter(Class<T> paramClass, String name) {
        return new ParameterExpressionImpl<>(paramClass, name);
    }

    @Override
    public <T> Expression<T> literal(T value) throws IllegalArgumentException{
        if (value == null) throw new IllegalArgumentException("Literal expression cannot be null.");
        return new ExpressionLiteralImpl<>(value);
    }

    @Override
    public Predicate and(Expression<Boolean> x, Expression<Boolean> y) {
        return new CompoundedPredicateImpl(Predicate.BooleanOperator.AND, Arrays.asList(x,y));
    }

    @Override
    public Predicate and(Predicate... restrictions) {
        return new CompoundedPredicateImpl(Predicate.BooleanOperator.AND, Arrays.asList(restrictions));
    }

    @Override
    public Predicate or(Expression<Boolean> x, Expression<Boolean> y) {
        return new CompoundedPredicateImpl(Predicate.BooleanOperator.OR, Arrays.asList(x,y));
    }

    @Override
    public Predicate or(Predicate... restrictions) {
        return new CompoundedPredicateImpl(Predicate.BooleanOperator.OR, Arrays.asList(restrictions));
    }

    @Override
    public Predicate equals(Expression<?> x, Expression<?> y) {
        return new SimplePredicateImpl(new ExpressionEqualsImpl<Boolean>((AbstractExpression<?>)x,(AbstractExpression<?>)y,this));
    }

    @Override
    public Predicate equals(Expression<?> x, Object y) {
        return new SimplePredicateImpl(new ExpressionEqualsImpl<Boolean>((AbstractExpression<?>) x, y,this));
    }

    @Override
    public Predicate notEquals(Expression<?> x, Expression<?> y) {
        return new SimplePredicateImpl(new ExpressionNotEqualsImpl((AbstractExpression<?>)x,(AbstractExpression<?>)y,this));
    }

    @Override
    public Predicate notEquals(Expression<?> x, Object y) {
        return new SimplePredicateImpl(new ExpressionNotEqualsImpl((AbstractExpression<?>)x,y,this));

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

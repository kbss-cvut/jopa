package cz.cvut.kbss.jopa.query.criteria;

import cz.cvut.kbss.jopa.model.CriteriaQueryImpl;
import cz.cvut.kbss.jopa.model.query.criteria.*;
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
        return new CriteriaQueryImpl<>(new CriteriaQueryHolder<>(resultClass), uow.getMetamodel(), this);
    }

    @Override
    public Expression<Integer> count(Expression<?> x) {
        if (x == null) throw new IllegalArgumentException("Aggregate function cannot be applied to null expression.");
        if (x instanceof AbstractPathExpression){
            if (x instanceof RootImpl) {
                RootImpl root = (RootImpl) x;
                return new ExpressionCountImpl(Integer.class,(AbstractPathExpression) root.getParentPath());
            } else{
                return new ExpressionCountImpl(Integer.class,(AbstractPathExpression) x);
            }
        }
        throw new IllegalArgumentException("Aggregate function can be applied only to path expressions.");
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
        if (value == null) throw new IllegalArgumentException("Literal created by this method cannot be null. Use nullLiteral method instead.");
        return new ExpressionLiteralImpl<>(value);
    }

    @Override
    public Expression<String> literal(String value, String languageTag) throws IllegalArgumentException{
        if (value == null) throw new IllegalArgumentException("Literal created by this method cannot be null. Use nullLiteral method instead.");
        return new ExpressionLiteralImpl<>(value,languageTag);
    }

    @Override
    public Order asc(Expression<?> x) {
        return new OrderImpl(x);
    }

    @Override
    public Order desc(Expression<?> x) {
        return new OrderImpl(x,false);
    }


    @Override
    public Predicate and(Expression<Boolean> x, Expression<Boolean> y) {
        return new CompoundedPredicateImpl(Predicate.BooleanOperator.AND, Arrays.asList(x,y));
    }

    @Override
    public Predicate and(Predicate... restrictions) {
        if (restrictions.length == 1) return new SimplePredicateImpl(restrictions[0]);
        else return new CompoundedPredicateImpl(Predicate.BooleanOperator.AND, Arrays.asList(restrictions));
    }

    @Override
    public Predicate or(Expression<Boolean> x, Expression<Boolean> y) {
        return new CompoundedPredicateImpl(Predicate.BooleanOperator.OR, Arrays.asList(x,y));
    }

    @Override
    public Predicate or(Predicate... restrictions) {
        if (restrictions.length == 1) return new SimplePredicateImpl(restrictions[0]);
        else return new CompoundedPredicateImpl(Predicate.BooleanOperator.OR, Arrays.asList(restrictions));
    }

    @Override
    public Predicate equal(Expression<?> x, Expression<?> y) {
        return new SimplePredicateImpl(new ExpressionEqualsImpl<>((AbstractExpression<?>)x,(AbstractExpression<?>)y));
    }

    @Override
    public Predicate equal(Expression<?> x, Object y) {
        return new SimplePredicateImpl(new ExpressionEqualsImpl<>((AbstractExpression<?>) x, new ExpressionLiteralImpl<>(y)));
    }

    @Override
    public Predicate equal(Expression<?> x, String y, String languageTag) {
        return new SimplePredicateImpl(new ExpressionEqualsImpl<>((AbstractExpression<?>) x, new ExpressionLiteralImpl<>(y,languageTag)));
    }

    @Override
    public Predicate notEqual(Expression<?> x, Expression<?> y) {
        return new SimplePredicateImpl(new ExpressionNotEqualsImpl<>((AbstractExpression<?>)x,(AbstractExpression<?>)y));
    }

    @Override
    public Predicate notEqual(Expression<?> x, Object y) {
        return new SimplePredicateImpl(new ExpressionNotEqualsImpl<>((AbstractExpression<?>)x, new ExpressionLiteralImpl<>(y)));

    }

    @Override
    public Predicate like(Expression<String> x, Expression<String> pattern) {
        return new SimplePredicateImpl(new ExpressionLikeImpl<>((AbstractExpression<String>)x,(AbstractExpression<String>)pattern));
    }

    @Override
    public Predicate like(Expression<String> x, String pattern) {
        return new SimplePredicateImpl(new ExpressionLikeImpl<>((AbstractExpression<String>)x, new ExpressionLiteralImpl<>(pattern)));
    }

    @Override
    public Predicate notLike(Expression<String> x, Expression<String> pattern) {
        return new SimplePredicateImpl(new ExpressionNotLikeImpl<>((AbstractExpression<String>)x,(AbstractExpression<String>)pattern));
    }

    @Override
    public Predicate notLike(Expression<String> x, String pattern) {
        return new SimplePredicateImpl(new ExpressionNotLikeImpl<>((AbstractExpression<String>)x, new ExpressionLiteralImpl<>(pattern)));
    }

    @Override
    public Predicate not(Expression<Boolean> restriction) {
        return wrapExpressionToPredicateWithRepair(restriction).not();
    }

    @Override
    public <Y extends Comparable<? super Y>> Predicate greaterThan(Expression<? extends Y> x, Expression<? extends Y> y) {
        return new SimplePredicateImpl(new ExpressionGreaterThanImpl<>((AbstractExpression<Y>)x, (AbstractExpression<Y>)y));
    }

    @Override
    public <Y extends Comparable<? super Y>> Predicate greaterThan(Expression<? extends Y> x, Y y) {
        return new SimplePredicateImpl(new ExpressionGreaterThanImpl<>((AbstractExpression<Y>)x, new ExpressionLiteralImpl<>(y)));
    }

    @Override
    public <Y extends Comparable<? super Y>> Predicate greaterThanOrEqual(Expression<? extends Y> x, Expression<? extends Y> y) {
        return new SimplePredicateImpl(new ExpressionGreaterThanOrEqualImpl<>((AbstractExpression<Y>)x, (AbstractExpression<Y>)y));
    }

    @Override
    public <Y extends Comparable<? super Y>> Predicate greaterThanOrEqual(Expression<? extends Y> x, Y y) {
        return new SimplePredicateImpl(new ExpressionGreaterThanOrEqualImpl<>((AbstractExpression<Y>)x, new ExpressionLiteralImpl<>(y)));
    }

    @Override
    public <Y extends Comparable<? super Y>> Predicate lessThan(Expression<? extends Y> x, Expression<? extends Y> y) {
        return new SimplePredicateImpl(new ExpressionLessThanImpl<>((AbstractExpression<Y>)x, (AbstractExpression<Y>)y));
    }

    @Override
    public <Y extends Comparable<? super Y>> Predicate lessThan(Expression<? extends Y> x, Y y) {
        return new SimplePredicateImpl(new ExpressionLessThanImpl<>((AbstractExpression<Y>)x, new ExpressionLiteralImpl<>(y)));
    }

    @Override
    public <Y extends Comparable<? super Y>> Predicate lessThanOrEqual(Expression<? extends Y> x, Expression<? extends Y> y) {
        return new SimplePredicateImpl(new ExpressionLessThanOrEqualImpl<>((AbstractExpression<Y>)x, (AbstractExpression<Y>)y));
    }

    @Override
    public <Y extends Comparable<? super Y>> Predicate lessThanOrEqual(Expression<? extends Y> x, Y y) {
        return new SimplePredicateImpl(new ExpressionLessThanOrEqualImpl<>((AbstractExpression<Y>)x, new ExpressionLiteralImpl<>(y)));
    }



    /**
     * Method wraps given boolean expression to Predicate and if path expression occur, it wrap it to ExpressionEqualsImpl before.
     * For example:
     * Expression<Boolean> expression = factory.get("attributeName");
     * Looks like boolean expression but in fact it is not boolean expression, so we need to fix this.
     * @param expression - boolean or path expression
     * @return Expression wraped in Predicate
     */
    public Predicate wrapExpressionToPredicateWithRepair(Expression<Boolean> expression){
        if (expression instanceof Predicate){
            return (Predicate)expression;
        } else if (expression instanceof AbstractPathExpression){
            return new SimplePredicateImpl(new ExpressionEqualsImpl<>((AbstractExpression<Boolean>) expression,null));
        } else {
            return new SimplePredicateImpl(expression);
        }
    }

}

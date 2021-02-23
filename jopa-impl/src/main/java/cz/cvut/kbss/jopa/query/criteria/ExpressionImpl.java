package cz.cvut.kbss.jopa.query.criteria;

import cz.cvut.kbss.jopa.model.query.criteria.Expression;
import cz.cvut.kbss.jopa.model.query.criteria.Predicate;

import java.util.Collection;

abstract class ExpressionImpl<Y> extends SelectionImpl<Y> implements Expression<Y> {

    public ExpressionImpl(Class<Y> type, Expression expression) {
        super(type,expression);
    }

    protected String getQueryPart(){
        return null;
    };

    @Override
    public Predicate in(Collection<?> values) {
        return null;
    }

    @Override
    public Predicate in(Expression<?> values) {
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
    public Predicate not() {
        return null;
    }

    @Override
    public boolean isNegated() {
        return false;
    }
}



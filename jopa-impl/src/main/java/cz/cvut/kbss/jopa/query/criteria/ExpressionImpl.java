package cz.cvut.kbss.jopa.query.criteria;

import cz.cvut.kbss.jopa.model.query.criteria.Expression;
import cz.cvut.kbss.jopa.model.query.criteria.Predicate;

import java.util.Collection;

abstract class ExpressionImpl<Y> extends SelectionImpl<Y> implements Expression<Y> {

    public ExpressionImpl(Class<Y> type, ExpressionImpl expression) {
        super(type, expression);
    }

    @Override
    public Predicate in(Collection<?> values) {
        return null;
    }

    @Override
    public Predicate in(Expression<?>... values) {
        return null;
    }

    @Override
    public Predicate in(Object... values) {
        return null;
    }

    @Override
    public Predicate isNotNull() {
        return null;
    }

    @Override
    public Predicate isNull() {
        return null;
    }
}



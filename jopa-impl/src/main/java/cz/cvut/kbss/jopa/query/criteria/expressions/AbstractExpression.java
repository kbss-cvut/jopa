package cz.cvut.kbss.jopa.query.criteria.expressions;

import cz.cvut.kbss.jopa.model.query.criteria.Expression;
import cz.cvut.kbss.jopa.model.query.criteria.Predicate;
import cz.cvut.kbss.jopa.query.criteria.SelectionImpl;

import java.util.Collection;

abstract public class AbstractExpression<Y> extends SelectionImpl<Y> implements Expression<Y> {

    public AbstractExpression(Class<Y> type) {
        super(type);
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

    abstract public void setExpressionToQuery(StringBuilder query);
}



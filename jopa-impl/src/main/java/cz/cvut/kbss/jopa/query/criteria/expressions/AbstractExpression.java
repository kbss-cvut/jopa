package cz.cvut.kbss.jopa.query.criteria.expressions;

import cz.cvut.kbss.jopa.model.query.criteria.Expression;
import cz.cvut.kbss.jopa.model.query.criteria.Predicate;
import cz.cvut.kbss.jopa.query.criteria.CriteriaParameterFiller;
import cz.cvut.kbss.jopa.query.criteria.SelectionImpl;

import java.util.Collection;

abstract public class AbstractExpression<Y> extends SelectionImpl<Y> implements Expression<Y> {

    protected boolean negated;

    public AbstractExpression(Class<Y> type) {
        super(type);
        negated = false;
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

    abstract public void setExpressionToQuery(StringBuilder query, CriteriaParameterFiller parameterFiller);

    public boolean isNegated(){
        return negated;
    }

    public void negate(){
        this.negated = true;
    }
}



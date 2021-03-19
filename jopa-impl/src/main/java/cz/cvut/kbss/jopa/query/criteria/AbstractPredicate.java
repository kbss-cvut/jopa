package cz.cvut.kbss.jopa.query.criteria;

import cz.cvut.kbss.jopa.model.query.criteria.Expression;
import cz.cvut.kbss.jopa.model.query.criteria.Predicate;
import cz.cvut.kbss.jopa.query.criteria.expressions.AbstractExpression;

import java.util.List;

abstract public class AbstractPredicate extends AbstractExpression<Boolean> implements Predicate {

    protected BooleanOperator booleanOperator;

    public AbstractPredicate(BooleanOperator booleanOperator) {
        super(Boolean.class);
        this.booleanOperator = booleanOperator;
    }

    @Override
    abstract public List<Expression<Boolean>> getExpressions();

    @Override
    public BooleanOperator getOperator() {
        return this.booleanOperator;
    }

    @Override
    public Predicate not() {
        return null;
    }

    @Override
    public boolean isNegated() {
        return false;
    }

    @Override
    public void setExpressionToQuery(StringBuilder query) {

    }
}

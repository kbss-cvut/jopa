package cz.cvut.kbss.jopa.query.criteria;

import cz.cvut.kbss.jopa.model.query.criteria.Expression;
import cz.cvut.kbss.jopa.model.query.criteria.Predicate;
import cz.cvut.kbss.jopa.query.criteria.expressions.AbstractExpression;
import cz.cvut.kbss.jopa.sessions.CriteriaBuilder;

import java.util.List;

abstract public class AbstractPredicate extends AbstractExpression<Boolean> implements Predicate {

    protected BooleanOperator booleanOperator;

    public AbstractPredicate(BooleanOperator booleanOperator, CriteriaBuilder cb) {
        super(Boolean.class, cb);
        this.booleanOperator = booleanOperator;
    }

    @Override
    abstract public List<Expression<Boolean>> getExpressions();

    @Override
    public BooleanOperator getOperator() {
        return this.booleanOperator;
    }

    @Override
    abstract public void setExpressionToQuery(StringBuilder query, CriteriaParameterFiller parameterFiller);

    protected void negateOperator(){
        if (booleanOperator.equals(BooleanOperator.AND)) booleanOperator = BooleanOperator.OR;
        else booleanOperator = BooleanOperator.AND;
    }


}

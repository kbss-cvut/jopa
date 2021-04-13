package cz.cvut.kbss.jopa.query.criteria.expressions;

import cz.cvut.kbss.jopa.query.criteria.CriteriaParameterFiller;
import cz.cvut.kbss.jopa.sessions.CriteriaFactory;

abstract public class AbstractComparisonExpression extends AbstractExpression<Boolean> {

    protected AbstractExpression<?> right;
    protected AbstractExpression<?> left;

    public AbstractComparisonExpression(AbstractExpression<?> x, AbstractExpression<?> y, CriteriaFactory factory) {
        super(Boolean.class, factory);
        this.left = x;
        this.right = y;
    }

    @Override
    public void setExpressionToQuery(StringBuilder query, CriteriaParameterFiller parameterFiller) {
        this.left.setExpressionToQuery(query, parameterFiller);
        query.append(this.getComparisonOperator());
        this.right.setExpressionToQuery(query, parameterFiller);
    }

    abstract protected String getComparisonOperator();
}



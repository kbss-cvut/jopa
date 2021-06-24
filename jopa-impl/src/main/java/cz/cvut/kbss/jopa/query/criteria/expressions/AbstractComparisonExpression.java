package cz.cvut.kbss.jopa.query.criteria.expressions;

import cz.cvut.kbss.jopa.query.criteria.CriteriaParameterFiller;
import cz.cvut.kbss.jopa.sessions.CriteriaBuilder;

abstract public class AbstractComparisonExpression extends AbstractExpression<Boolean> {

    protected AbstractExpression<?> right;
    protected AbstractExpression<?> left;

    public AbstractComparisonExpression(AbstractExpression<?> x, AbstractExpression<?> y, CriteriaBuilder cb) {
        super(Boolean.class, cb);
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



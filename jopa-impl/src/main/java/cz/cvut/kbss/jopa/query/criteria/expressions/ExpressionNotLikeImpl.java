package cz.cvut.kbss.jopa.query.criteria.expressions;

import cz.cvut.kbss.jopa.sessions.CriteriaFactory;

public class ExpressionNotLikeImpl extends AbstractComparisonExpression {


    public ExpressionNotLikeImpl(AbstractExpression<?> x, AbstractExpression<?> y, CriteriaFactory factory) {
        super(x, y,factory);
    }


    @Override
    protected String getComparisonOperator() {
        return this.isNegated() ? " LIKE " : " NOT LIKE ";
    }
}



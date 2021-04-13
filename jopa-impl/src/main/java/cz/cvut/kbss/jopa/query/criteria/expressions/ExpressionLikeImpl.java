package cz.cvut.kbss.jopa.query.criteria.expressions;

import cz.cvut.kbss.jopa.sessions.CriteriaFactory;

public class ExpressionLikeImpl extends AbstractComparisonExpression {


    public ExpressionLikeImpl(AbstractExpression<?> x, AbstractExpression<?> y, CriteriaFactory factory) {
        super(x, y,factory);
    }

    @Override
    protected String getComparisonOperator() {
        return this.isNegated() ? " NOT LIKE " : " LIKE ";
    }
}



package cz.cvut.kbss.jopa.query.criteria.expressions;

import cz.cvut.kbss.jopa.sessions.CriteriaBuilder;

public class ExpressionLikeImpl extends AbstractComparisonExpression {


    public ExpressionLikeImpl(AbstractExpression<?> x, AbstractExpression<?> y, CriteriaBuilder cb) {
        super(x, y,cb);
    }

    @Override
    protected String getComparisonOperator() {
        return this.isNegated() ? " NOT LIKE " : " LIKE ";
    }
}



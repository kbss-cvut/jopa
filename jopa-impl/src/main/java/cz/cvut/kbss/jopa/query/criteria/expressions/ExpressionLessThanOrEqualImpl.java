package cz.cvut.kbss.jopa.query.criteria.expressions;


import cz.cvut.kbss.jopa.sessions.CriteriaFactory;

public class ExpressionLessThanOrEqualImpl extends AbstractComparisonExpression {

    public ExpressionLessThanOrEqualImpl(AbstractExpression<?> x, AbstractExpression<?> y,  CriteriaFactory factory) {
        super(x, y, factory);
    }

    @Override
    protected String getComparisonOperator() {
        return this.isNegated() ? " > " : " <= ";
    }
}



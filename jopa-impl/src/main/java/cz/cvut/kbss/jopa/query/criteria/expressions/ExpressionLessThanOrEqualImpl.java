package cz.cvut.kbss.jopa.query.criteria.expressions;


public class ExpressionLessThanOrEqualImpl<Y> extends AbstractComparisonExpression<Y> {

    public ExpressionLessThanOrEqualImpl(AbstractExpression<?> x, AbstractExpression<?> y) {
        super(x, y);
    }

    @Override
    protected String getComparisonOperator() {
        return this.isNegated() ? " > " : " <= ";
    }
}



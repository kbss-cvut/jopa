package cz.cvut.kbss.jopa.query.criteria.expressions;


public class ExpressionGreaterThanOrEqualImpl<Y> extends AbstractComparisonExpression<Y> {

    public ExpressionGreaterThanOrEqualImpl(AbstractExpression<?> x, AbstractExpression<?> y) {
        super(x, y);
    }

    @Override
    protected String getComparisonOperator() {
        return this.isNegated() ? " < " : " >= ";
    }
}



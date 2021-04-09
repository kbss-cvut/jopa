package cz.cvut.kbss.jopa.query.criteria.expressions;


public class ExpressionLessThanImpl<Y> extends AbstractComparisonExpression<Y> {

    public ExpressionLessThanImpl(AbstractExpression<?> x, AbstractExpression<?> y) {
        super(x, y);
    }

    @Override
    protected String getComparisonOperator() {
        return this.isNegated() ? " >= " : " < ";
    }
}



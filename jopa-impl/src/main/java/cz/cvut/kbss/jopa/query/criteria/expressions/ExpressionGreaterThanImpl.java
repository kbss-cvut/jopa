package cz.cvut.kbss.jopa.query.criteria.expressions;


public class ExpressionGreaterThanImpl<Y> extends AbstractComparisonExpression<Y> {

    public ExpressionGreaterThanImpl(AbstractExpression<?> x, AbstractExpression<?> y) {
        super(x, y);
    }

    @Override
    protected String getComparisonOperator() {
        return this.isNegated() ? " <= " : " > ";
    }
}



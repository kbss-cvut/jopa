package cz.cvut.kbss.jopa.query.criteria.expressions;

public class ExpressionNotLikeImpl<Y> extends AbstractComparisonExpression<Y> {


    public ExpressionNotLikeImpl(AbstractExpression<?> x, AbstractExpression<?> y) {
        super(x, y);
    }


    @Override
    protected String getComparisonOperator() {
        return this.isNegated() ? " LIKE " : " NOT LIKE ";
    }
}



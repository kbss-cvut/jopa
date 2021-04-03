package cz.cvut.kbss.jopa.query.criteria.expressions;

public class ExpressionLikeImpl<Y> extends AbstractComparisonExpression<Y> {


    public ExpressionLikeImpl(AbstractExpression<?> x, AbstractExpression<?> y) {
        super(x, y);
    }


    @Override
    protected String getComparisonOperator() {
        return this.isNegated() ? " NOT LIKE " : " LIKE ";
    }
}



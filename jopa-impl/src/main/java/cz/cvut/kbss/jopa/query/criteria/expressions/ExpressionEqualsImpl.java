package cz.cvut.kbss.jopa.query.criteria.expressions;

public class ExpressionEqualsImpl<Y> extends AbstractComparisonExpression<Y> {


    public ExpressionEqualsImpl(AbstractExpression<?> x, AbstractExpression<?> y) {
        super(x, y);
    }


    @Override
    protected String getComparisonOperator() {
        return this.isNegated() ? " <> " : " = ";
    }
}



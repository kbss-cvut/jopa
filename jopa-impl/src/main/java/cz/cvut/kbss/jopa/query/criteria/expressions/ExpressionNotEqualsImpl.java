package cz.cvut.kbss.jopa.query.criteria.expressions;

public class ExpressionNotEqualsImpl<Y> extends AbstractComparisonExpression<Y> {


    public ExpressionNotEqualsImpl(AbstractExpression<?> x, AbstractExpression<?> y) {
        super(x, y);
    }

    @Override
    protected String getComparisonOperator() {
        return this.isNegated() ? " = " : " <> ";
    }
}



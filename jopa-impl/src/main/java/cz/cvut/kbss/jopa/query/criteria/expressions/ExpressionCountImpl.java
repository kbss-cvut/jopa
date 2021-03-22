package cz.cvut.kbss.jopa.query.criteria.expressions;

public class ExpressionCountImpl<Y> extends AbstractAggregateFunctionExpression<Y> {
    public ExpressionCountImpl(Class<Y> type, AbstractPathExpression expression) {
        super(type, expression);
    }

    @Override
    public String getFunctionName() {
        return "COUNT";
    }
}

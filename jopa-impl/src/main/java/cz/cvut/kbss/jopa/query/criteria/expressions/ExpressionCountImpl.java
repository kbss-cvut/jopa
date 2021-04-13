package cz.cvut.kbss.jopa.query.criteria.expressions;

import cz.cvut.kbss.jopa.sessions.CriteriaFactory;

public class ExpressionCountImpl<Y> extends AbstractAggregateFunctionExpression<Y> {
    public ExpressionCountImpl(Class<Y> type, AbstractPathExpression expression, CriteriaFactory factory) {
        super(type, expression, factory);
    }

    @Override
    public String getFunctionName() {
        return "COUNT";
    }
}

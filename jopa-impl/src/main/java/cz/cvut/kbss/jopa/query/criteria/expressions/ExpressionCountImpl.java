package cz.cvut.kbss.jopa.query.criteria.expressions;

import cz.cvut.kbss.jopa.sessions.CriteriaBuilder;

public class ExpressionCountImpl<Y> extends AbstractAggregateFunctionExpression<Y> {
    public ExpressionCountImpl(Class<Y> type, AbstractPathExpression expression, CriteriaBuilder cb) {
        super(type, expression, cb);
    }

    @Override
    public String getFunctionName() {
        return "COUNT";
    }
}

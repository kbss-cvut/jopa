package cz.cvut.kbss.jopa.query.criteria.expressions;

import cz.cvut.kbss.jopa.sessions.CriteriaFactory;

public class ExpressionEqualsImpl<Y> extends AbstractComparisonExpression<Y> {


    public ExpressionEqualsImpl(AbstractExpression<?> x, AbstractExpression<?> y) {
        super(x, y);
    }


    @Override
    protected String getComparisonOperator() {
        return " = ";
    }
}



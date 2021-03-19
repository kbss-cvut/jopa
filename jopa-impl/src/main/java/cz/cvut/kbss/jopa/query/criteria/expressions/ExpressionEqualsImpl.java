package cz.cvut.kbss.jopa.query.criteria.expressions;

import cz.cvut.kbss.jopa.sessions.CriteriaFactory;

public class ExpressionEqualsImpl<Y> extends AbstractComparisonExpression<Y> {


    public ExpressionEqualsImpl(AbstractExpression<?> x, AbstractExpression<?> y, CriteriaFactory factory ) {
        super(x, y, factory);
    }

    public ExpressionEqualsImpl(AbstractExpression<?> x, Object y, CriteriaFactory factory) {
        super(x, y, factory);
    }

    @Override
    protected String getComparisonOperator() {
        return " = ";
    }
}



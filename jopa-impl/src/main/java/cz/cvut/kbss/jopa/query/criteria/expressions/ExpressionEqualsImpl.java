package cz.cvut.kbss.jopa.query.criteria.expressions;

import cz.cvut.kbss.jopa.sessions.CriteriaFactory;

public class ExpressionEqualsImpl extends AbstractComparisonExpression {


    public ExpressionEqualsImpl(AbstractExpression<?> x, AbstractExpression<?> y,  CriteriaFactory factory) {
        super(x, y, factory);
    }


    @Override
    protected String getComparisonOperator() {
        return this.isNegated() ? " <> " : " = ";
    }
}



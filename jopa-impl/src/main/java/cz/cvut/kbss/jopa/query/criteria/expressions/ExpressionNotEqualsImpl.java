package cz.cvut.kbss.jopa.query.criteria.expressions;


import cz.cvut.kbss.jopa.sessions.CriteriaFactory;

public class ExpressionNotEqualsImpl<Y> extends AbstractComparisonExpression<Y> {


    public ExpressionNotEqualsImpl(AbstractExpression<?> x, AbstractExpression<?> y) {
        super(x, y);
    }

    @Override
    protected String getComparisonOperator() {
        return " <> ";
    }
}



package cz.cvut.kbss.jopa.query.criteria.expressions;


import cz.cvut.kbss.jopa.sessions.CriteriaFactory;

public class ExpressionNotEqualsImpl<Y> extends AbstractComparisonExpression<Y> {


    public ExpressionNotEqualsImpl(AbstractExpression<?> x, AbstractExpression<?> y, CriteriaFactory factory) {
        super(x, y, factory);
    }

    public ExpressionNotEqualsImpl(AbstractExpression<?> x, Object y, CriteriaFactory factory) {
        super(x, y, factory);
    }

    @Override
    protected String getComparisonOperator() {
        return " <> ";
    }
}



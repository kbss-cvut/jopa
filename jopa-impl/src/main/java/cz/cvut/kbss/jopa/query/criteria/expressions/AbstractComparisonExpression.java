package cz.cvut.kbss.jopa.query.criteria.expressions;

import cz.cvut.kbss.jopa.query.criteria.CriteriaFactoryImpl;
import cz.cvut.kbss.jopa.sessions.CriteriaFactory;

abstract public class AbstractComparisonExpression<Y> extends AbstractExpression<Y> {

    protected AbstractExpression<?> right;
    protected AbstractExpression<?> left;
    protected Object literal;
    protected final CriteriaFactory factory;

    public AbstractComparisonExpression(AbstractExpression<Y> x, AbstractExpression<?> y, CriteriaFactory factory) {
        super(null);
        this.left = x;
        this.right = y;
        this.factory = factory;
        this.literal = null;
    }

    public AbstractComparisonExpression(AbstractExpression<?> x, Object y, CriteriaFactory factory) {
        super(null);
        this.left = x;
        this.factory = factory;
        this.right = null;
        this.literal = (ExpressionLiteralImpl) factory.literal(y);
    }

    @Override
    public void setExpressionToQuery(StringBuilder query) {
        this.left.setExpressionToQuery(query);
        query.append(this.getComparisonOperator());
        if (right != null){
            this.right.setExpressionToQuery(query);
        } else {
            query.append(this.literal.toString());
        }
    }

    abstract protected String getComparisonOperator();
}



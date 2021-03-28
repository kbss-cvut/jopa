package cz.cvut.kbss.jopa.query.criteria.expressions;

import cz.cvut.kbss.jopa.query.criteria.CriteriaFactoryImpl;
import cz.cvut.kbss.jopa.query.criteria.CriteriaParameterFiller;
import cz.cvut.kbss.jopa.sessions.CriteriaFactory;

abstract public class AbstractComparisonExpression<Y> extends AbstractExpression<Y> {

    protected AbstractExpression<?> right;
    protected AbstractExpression<?> left;
    protected ExpressionLiteralImpl literal;
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

    public AbstractComparisonExpression(AbstractExpression<?> x, String y, String languageTag, CriteriaFactory factory) {
        super(null);
        this.left = x;
        this.factory = factory;
        this.right = null;
        this.literal = (ExpressionLiteralImpl) factory.literal(y,languageTag);
    }

    @Override
    public void setExpressionToQuery(StringBuilder query, CriteriaParameterFiller parameterFiller) {
        this.left.setExpressionToQuery(query, parameterFiller);
        query.append(this.getComparisonOperator());
        if (right != null){
            this.right.setExpressionToQuery(query, parameterFiller);
        } else {
            query.append(parameterFiller.registerParameter(literal));
        }
    }

    abstract protected String getComparisonOperator();
}



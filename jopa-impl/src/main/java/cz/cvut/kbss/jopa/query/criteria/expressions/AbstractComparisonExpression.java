package cz.cvut.kbss.jopa.query.criteria.expressions;

import cz.cvut.kbss.jopa.query.criteria.CriteriaParameterFiller;

abstract public class AbstractComparisonExpression<Y> extends AbstractExpression<Y> {

    protected AbstractExpression<?> right;
    protected AbstractExpression<?> left;

    public AbstractComparisonExpression(AbstractExpression<?> x, AbstractExpression<?> y) {
        super(null);
        this.left = x;
        this.right = y;
    }

    @Override
    public void setExpressionToQuery(StringBuilder query, CriteriaParameterFiller parameterFiller) {
        this.left.setExpressionToQuery(query, parameterFiller);
        query.append(this.getComparisonOperator());
        //TODO - BAKALARKA - KONZULTACIA
        // instanceof alebo radsej nech maju expressions napr. metodu isLiteral()?
        if (right instanceof ExpressionLiteralImpl){
            query.append(parameterFiller.registerParameter((ExpressionLiteralImpl) right));
        } else{
            this.right.setExpressionToQuery(query, parameterFiller);
        }
    }

    abstract protected String getComparisonOperator();
}



package cz.cvut.kbss.jopa.query.criteria.expressions;

import cz.cvut.kbss.jopa.query.criteria.CriteriaParameterFiller;

abstract public class AbstractAggregateFunctionExpression<X> extends AbstractExpression<X> {

    protected AbstractPathExpression internExpression;

    public AbstractAggregateFunctionExpression(Class<X> type, AbstractPathExpression internExpression) {
        super(type);
        this.internExpression = internExpression;
    }

    @Override
    public void setExpressionToQuery(StringBuilder query, CriteriaParameterFiller parameterFiller) {
        query.append(this.getFunctionName() + "(");
        this.internExpression.setExpressionToQuery(query, parameterFiller);
        query.append(")");
    }

    abstract public String getFunctionName();

}



package cz.cvut.kbss.jopa.query.criteria.expressions;

import cz.cvut.kbss.jopa.query.criteria.CriteriaParameterFiller;

abstract public class AbstractAggregateFunctionExpression<X> extends AbstractExpression<X>{

    protected AbstractPathExpression pathSource;

    public AbstractAggregateFunctionExpression(Class<X> type, AbstractPathExpression pathSource) {
        super(type);
        this.pathSource = pathSource;
    }

    @Override
    public void setExpressionToQuery(StringBuilder query, CriteriaParameterFiller parameterFiller) {
        if (this.pathSource != null){
            throw new IllegalArgumentException("Aggregate function cannot be applied to null expression.");
        } else {
            query.append(this.getFunctionName()+"(");
            this.pathSource.setExpressionToQuery(query, parameterFiller);
            query.append(")");
        }
    }

    abstract public String getFunctionName();

}



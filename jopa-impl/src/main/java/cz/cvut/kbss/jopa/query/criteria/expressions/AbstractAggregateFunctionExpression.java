package cz.cvut.kbss.jopa.query.criteria.expressions;

import cz.cvut.kbss.jopa.query.criteria.CriteriaParameterFiller;
import cz.cvut.kbss.jopa.sessions.CriteriaBuilder;

public abstract class AbstractAggregateFunctionExpression<X> extends AbstractExpression<X> {

    protected AbstractPathExpression internExpression;

    public AbstractAggregateFunctionExpression(Class<X> type, AbstractPathExpression internExpression, CriteriaBuilder cb) {
        super(type, cb);
        this.internExpression = internExpression;
    }

    @Override
    public void setExpressionToQuery(StringBuilder query, CriteriaParameterFiller parameterFiller) {
        query.append(this.getFunctionName()).append("(");
        this.internExpression.setExpressionToQuery(query, parameterFiller);
        query.append(")");
    }

    public abstract String getFunctionName();
}



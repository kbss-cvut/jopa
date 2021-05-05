package cz.cvut.kbss.jopa.query.criteria.expressions;

import cz.cvut.kbss.jopa.query.criteria.CriteriaParameterFiller;
import cz.cvut.kbss.jopa.sessions.CriteriaBuilder;

public class ExpressionEqualImpl extends AbstractComparisonExpression {


    public ExpressionEqualImpl(AbstractExpression<?> x, AbstractExpression<?> y, CriteriaBuilder cb) {
        super(x, y, cb);
    }

    //TODO - remove this override implementation when SOQL will support equal negation as != or <>
    @Override
    public void setExpressionToQuery(StringBuilder query, CriteriaParameterFiller parameterFiller) {
        if (this.negated) query.append("NOT ");
        super.setExpressionToQuery(query, parameterFiller);
    }

    @Override
    protected String getComparisonOperator() {
        //TODO - change when SOQL will support equal negation as != or <>
        //return this.isNegated() ? " != " : " = ";
        return " = ";
    }
}



package cz.cvut.kbss.jopa.query.criteria.expressions;

import cz.cvut.kbss.jopa.query.criteria.CriteriaParameterFiller;
import cz.cvut.kbss.jopa.sessions.CriteriaFactory;

public class ExpressionNotEqualImpl extends AbstractComparisonExpression {


    public ExpressionNotEqualImpl(AbstractExpression<?> x, AbstractExpression<?> y, CriteriaFactory factory) {
        super(x, y, factory);
    }

    //TODO - remove this override implementation when SOQL will support equal negation as != or <>
    @Override
    public void setExpressionToQuery(StringBuilder query, CriteriaParameterFiller parameterFiller) {
        if (!this.negated) query.append("NOT ");
        this.left.setExpressionToQuery(query, parameterFiller);
        query.append(this.getComparisonOperator());
        this.right.setExpressionToQuery(query, parameterFiller);
    }

    @Override
    protected String getComparisonOperator() {
        //TODO - change when SOQL will support equal negation as != or <>
        //return this.isNegated() ? " = " : " != ";
        return " = ";
    }
}



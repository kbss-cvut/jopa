package cz.cvut.kbss.jopa.query.criteria.expressions;

import cz.cvut.kbss.jopa.query.criteria.CriteriaParameterFiller;

public class ExpressionLiteralImpl<T> extends AbstractExpression<T>  {

    protected Object literal;

    public ExpressionLiteralImpl(T literal) {
        super(determineClass(literal));
        this.literal = literal;
    }

    @Override
    public void setExpressionToQuery(StringBuilder query, CriteriaParameterFiller parameterFiller) {
        query.append(parameterFiller.registerParameter(this));
    }

    private static Class determineClass(Object literal) {
        return literal.getClass();
    }

    public Object getValue(){
        return literal;
    }

}

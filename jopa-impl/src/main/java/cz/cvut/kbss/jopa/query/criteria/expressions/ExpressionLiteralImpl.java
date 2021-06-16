package cz.cvut.kbss.jopa.query.criteria.expressions;

import cz.cvut.kbss.jopa.query.criteria.CriteriaParameterFiller;
import cz.cvut.kbss.jopa.sessions.CriteriaBuilder;

public class ExpressionLiteralImpl<T> extends AbstractExpression<T>  {

    private final Object literal;
    private final String languageTag;

    public ExpressionLiteralImpl(T literal, CriteriaBuilder cb) {
        super(determineClass(literal), cb);
        this.literal = literal;
        languageTag = null;
    }

    public ExpressionLiteralImpl(String literal, String languageTag, CriteriaBuilder cb) {
        super(determineClass(literal), cb);
        this.literal = literal;
        this.languageTag = languageTag;
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

    public String getLanguageTag(){
        return languageTag;
    }

}

package cz.cvut.kbss.jopa.query.criteria.expressions;

import cz.cvut.kbss.jopa.model.query.criteria.CriteriaBuilder;
import cz.cvut.kbss.jopa.query.criteria.CriteriaParameterFiller;
import cz.cvut.kbss.jopa.query.soql.SoqlConstants;

public class LangMatchesFunction extends AbstractExpression<Boolean> {

    private final AbstractExpression<String> value;
    private final AbstractExpression<String> range;

    public LangMatchesFunction(CriteriaBuilder cb, AbstractExpression<String> value, AbstractExpression<String> range) {
        super(Boolean.class, cb);
        this.value = value;
        this.range = range;
    }

    @Override
    public void setExpressionToQuery(StringBuilder query, CriteriaParameterFiller parameterFiller) {
        query.append(SoqlConstants.Functions.LANG_MATCHES)
                .append('(');
        value.setExpressionToQuery(query, parameterFiller);
        query.append(", ");
        range.setExpressionToQuery(query, parameterFiller);
        query.append(')');
    }
}

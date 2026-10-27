package cz.cvut.kbss.jopa.query.criteria.expressions;

import cz.cvut.kbss.jopa.query.criteria.CriteriaBuilderImpl;
import cz.cvut.kbss.jopa.query.soql.SoqlConstants;

public class RegexFunction extends AbstractFunctionExpression<Boolean> {

    public RegexFunction(CriteriaBuilderImpl cb, AbstractExpression... arguments) {
        super(Boolean.class, cb, arguments);
    }

    @Override
    public String getFunctionName() {
        return SoqlConstants.Functions.REGEX;
    }
}

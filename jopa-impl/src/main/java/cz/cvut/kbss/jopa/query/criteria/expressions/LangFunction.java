package cz.cvut.kbss.jopa.query.criteria.expressions;

import cz.cvut.kbss.jopa.query.soql.SoqlConstants;
import cz.cvut.kbss.jopa.sessions.CriteriaBuilder;

/**
 * Represents the SOQL {@link SoqlConstants.Functions#LANG} function.
 */
public class LangFunction extends AbstractFunctionExpression<String> {

    public LangFunction(AbstractPathExpression internExpression, CriteriaBuilder cb) {
        super(String.class, internExpression, cb);
    }

    @Override
    public String getFunctionName() {
        return SoqlConstants.Functions.LANG;
    }
}

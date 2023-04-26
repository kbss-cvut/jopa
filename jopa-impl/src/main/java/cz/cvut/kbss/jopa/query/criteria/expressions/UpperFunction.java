package cz.cvut.kbss.jopa.query.criteria.expressions;

import cz.cvut.kbss.jopa.query.soql.SoqlConstants;
import cz.cvut.kbss.jopa.sessions.CriteriaBuilder;

public class UpperFunction extends AbstractFunctionExpression<String> {

    public UpperFunction(AbstractPathExpression internExpression, CriteriaBuilder cb) {
        super(String.class, internExpression, cb);
    }

    @Override
    public String getFunctionName() {
        return SoqlConstants.Functions.UPPER;
    }
}

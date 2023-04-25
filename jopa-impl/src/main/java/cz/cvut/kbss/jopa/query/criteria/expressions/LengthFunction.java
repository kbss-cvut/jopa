package cz.cvut.kbss.jopa.query.criteria.expressions;

import cz.cvut.kbss.jopa.query.soql.SoqlConstants;
import cz.cvut.kbss.jopa.sessions.CriteriaBuilder;

public class LengthFunction extends AbstractFunctionExpression<Integer> {

    public LengthFunction(AbstractPathExpression argumentExpression, CriteriaBuilder cb) {
        super(Integer.class, argumentExpression, cb);
    }

    @Override
    public String getFunctionName() {
        return SoqlConstants.Functions.LENGTH;
    }
}

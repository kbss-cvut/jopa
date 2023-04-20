package cz.cvut.kbss.jopa.query.criteria.expressions;

import cz.cvut.kbss.jopa.query.soql.SoqlConstants;
import cz.cvut.kbss.jopa.sessions.CriteriaBuilder;

public class CeilFunction<Y extends Number> extends AbstractFunctionExpression<Y> {

    public CeilFunction(Class<Y> type, AbstractPathExpression argumentExpression, CriteriaBuilder cb) {
        super(type, argumentExpression, cb);
    }

    @Override
    public String getFunctionName() {
        return SoqlConstants.Functions.CEIL;
    }
}

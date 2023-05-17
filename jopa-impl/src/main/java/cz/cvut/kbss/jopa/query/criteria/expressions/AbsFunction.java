package cz.cvut.kbss.jopa.query.criteria.expressions;

import cz.cvut.kbss.jopa.query.soql.SoqlConstants;
import cz.cvut.kbss.jopa.sessions.CriteriaBuilder;

public class AbsFunction<Y extends Number> extends AbstractFunctionExpression<Y> {

    public AbsFunction(Class<Y> type, AbstractPathExpression argumentExpression, CriteriaBuilder cb) {
        super(type, argumentExpression, cb);
    }

    @Override
    public String getFunctionName() {
        return SoqlConstants.Functions.ABS;
    }
}

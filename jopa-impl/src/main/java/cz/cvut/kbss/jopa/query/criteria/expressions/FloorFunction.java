package cz.cvut.kbss.jopa.query.criteria.expressions;

import cz.cvut.kbss.jopa.query.soql.SoqlConstants;
import cz.cvut.kbss.jopa.sessions.CriteriaBuilder;

public class FloorFunction<Y extends Number> extends AbstractFunctionExpression<Y> {

    public FloorFunction(Class<Y> type, AbstractPathExpression argumentExpression, CriteriaBuilder cb) {
        super(type, argumentExpression, cb);
    }

    @Override
    public String getFunctionName() {
        return SoqlConstants.Functions.FLOOR;
    }
}

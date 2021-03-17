package cz.cvut.kbss.jopa.query.criteria.expressions;

import cz.cvut.kbss.jopa.model.metamodel.Metamodel;

public class ExpressionFromImpl<Y> extends AbstractPathExpression<Y> {


    public ExpressionFromImpl(Class<Y> type, AbstractPathExpression pathSource, Metamodel metamodel) {
        super(type, pathSource, metamodel);
    }

    @Override
    public void setExpressionToQuery(StringBuilder query) {
        if (this.pathSource != null){
            this.pathSource.setExpressionToQuery(query);
            query.append("." + type.getSimpleName().toLowerCase());
        } else {
            query.append(type.getSimpleName().toLowerCase());
        }
    }
}



package cz.cvut.kbss.jopa.query.criteria;

import cz.cvut.kbss.jopa.model.query.criteria.Expression;

public class ExpressionEntityImpl<Y> extends ExpressionImpl<Y> {
    public ExpressionEntityImpl(Class<Y> type) {
        super(type,null);
    }

    @Override
    public void setExpressionToQuery(StringBuilder query) {
        if (this.expression != null){
            this.expression.setExpressionToQuery(query);
            query.append("." + type.getSimpleName().toLowerCase());
        } else {
            query.append(type.getSimpleName().toLowerCase());
        }
    }
}



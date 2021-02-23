package cz.cvut.kbss.jopa.query.criteria;

import cz.cvut.kbss.jopa.model.query.criteria.Expression;

public class ExpressionAttributeImpl<Y> extends ExpressionImpl<Y>{

    protected String attributeName;

    public ExpressionAttributeImpl(String attributeName) {
        super(null,null);
        this.attributeName = attributeName;
    }

    public ExpressionAttributeImpl(String attributeName, Expression expression) {
        super(null,expression);
        this.attributeName = attributeName;
    }
}

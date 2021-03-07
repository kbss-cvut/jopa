package cz.cvut.kbss.jopa.query.criteria;

import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.query.criteria.Expression;

public class ExpressionAttributeImpl<Y> extends ExpressionImpl<Y>{

    protected String attributeName;
    protected Attribute attribute;

    public ExpressionAttributeImpl(Attribute attribute) {
        super(null,null);
        this.attribute = attribute;
        this.attributeName = attribute.getName();
    }

    public ExpressionAttributeImpl(Attribute attribute, ExpressionImpl expression) {
        super(null,expression);
        this.attribute = attribute;
        this.attributeName = attribute.getName();
    }

    @Override
    public String getString() {
        return attributeName;
    }
}

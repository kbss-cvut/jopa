package cz.cvut.kbss.jopa.query.criteria;

import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.ManagedType;
import cz.cvut.kbss.jopa.model.query.criteria.Expression;
import cz.cvut.kbss.jopa.model.query.criteria.Path;
import cz.cvut.kbss.jopa.model.query.criteria.Selection;

public class PathImpl<X> extends ExpressionImpl<X> implements Path<X> {

    private final ManagedType<X> managedType;

    public PathImpl(ManagedType<X> managedType, Class<X> type) {
        super(type, new ExpressionEntityImpl<>(type));
        this.managedType = managedType;
    }

    public PathImpl(Attribute attribute, ExpressionImpl expression){
        super(null, new ExpressionAttributeImpl(attribute, expression));
        //TODO - how to get type "department" in this example -> person.getAttr("department").getAttr("name")
        this.managedType = null;
    }


    protected String getQueryPart() {
        return null;
    }

    @Override
    public <Y> Path<Y> getAttr(String attributeName) throws IllegalArgumentException {
        Attribute attribute = managedType.getAttribute(attributeName);
        Path<Y> nextExpression = new PathImpl<Y>(attribute, this.expression);
        this.expression = (ExpressionImpl) nextExpression;
        return nextExpression;
    }

    @Override
    public Path<?> getParentPath() {
        return null;
    }

    @Override
    public Expression<Class<? extends X>> type() {
        return null;
    }
}

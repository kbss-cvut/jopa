package cz.cvut.kbss.jopa.query.criteria;

import cz.cvut.kbss.jopa.model.metamodel.*;
import cz.cvut.kbss.jopa.model.query.criteria.Expression;
import cz.cvut.kbss.jopa.model.query.criteria.Path;
import cz.cvut.kbss.jopa.model.query.criteria.Selection;

public class PathImpl<X> extends ExpressionImpl<X> implements Path<X> {

    protected final Metamodel metamodel;

    public PathImpl(Metamodel metamodel, Class<X> type) {
        super(type, new ExpressionEntityImpl<>(type));
        this.metamodel = metamodel;
    }

    public PathImpl(Metamodel metamodel, Attribute attribute, ExpressionImpl expression) {
        super(null, new ExpressionAttributeImpl(attribute, expression));
        this.metamodel = metamodel;
    }


    protected String getQueryPart() {
        return null;
    }

    @Override
    public <Y> Path<Y> getAttr(String attributeName) throws IllegalArgumentException {
        Attribute attribute = metamodel.entity(type).getAttribute(attributeName);
        Path<Y> nextExpression = new PathImpl<Y>(this.metamodel, attribute, this.expression);
        this.expression = (ExpressionImpl) nextExpression;
        return nextExpression;
    }

    @Override
    public <Y> Path<Y> getAttr(SingularAttribute<? super X, Y> attribute) {
        Path<Y> nextExpression = new PathImpl<Y>(this.metamodel, attribute, this.expression);
        this.expression = (ExpressionImpl) nextExpression;
        return nextExpression;
    }

    @Override
    public Path<?> getParentPath() {
        return null;
    }

    @Override
    public String getString() {
        return expression.getString();
    }
}

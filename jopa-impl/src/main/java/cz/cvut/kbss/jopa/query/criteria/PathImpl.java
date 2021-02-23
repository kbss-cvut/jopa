package cz.cvut.kbss.jopa.query.criteria;

import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.query.criteria.Expression;
import cz.cvut.kbss.jopa.model.query.criteria.Path;
import cz.cvut.kbss.jopa.model.query.criteria.Selection;

public class PathImpl<X> extends ExpressionImpl<X> implements Path<X> {

    public PathImpl(Class<X> type) {
        super(type, new ExpressionEntityImpl<>(type));
    }

    public PathImpl(String attributeName) {
        super(null, new ExpressionAttributeImpl<>(attributeName));
    }

    public PathImpl(String attributeName, Expression expression){
        super(null, new ExpressionAttributeImpl<>(attributeName, expression));
    }


    protected String getQueryPart() {
        return null;
    }

    @Override
    public <Y> Path<Y> getAttr(String attributeName) throws IllegalArgumentException {
        Path<Y> nextExpression = new PathImpl<Y>(attributeName, this.expression);
        this.expression = nextExpression;
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

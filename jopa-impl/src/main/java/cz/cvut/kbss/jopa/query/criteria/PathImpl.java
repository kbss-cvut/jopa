package cz.cvut.kbss.jopa.query.criteria;

import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.query.criteria.Expression;
import cz.cvut.kbss.jopa.model.query.criteria.Path;
import cz.cvut.kbss.jopa.model.query.criteria.Selection;

public class PathImpl<X> extends ExpressionImpl<X> implements Path<X> {


    public <Y> PathImpl(Attribute<X, Y> attribute, ExpressionType expressionType, X value) {
        super(attribute, expressionType, value);
    }

    @Override
    public <Y> Path<Y> getAttr(String attributeName) throws IllegalArgumentException {
        return null;
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

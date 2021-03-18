package cz.cvut.kbss.jopa.query.criteria.expressions;

import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.model.metamodel.SingularAttribute;
import cz.cvut.kbss.jopa.model.query.criteria.Path;
import cz.cvut.kbss.jopa.query.criteria.PathImpl;

abstract public class AbstractPathExpression<X> extends AbstractExpression<X> implements Path<X> {

    protected AbstractPathExpression pathSource;
    protected final Metamodel metamodel;

    public AbstractPathExpression(Class<X> type, AbstractPathExpression pathSource, Metamodel metamodel) {
        super(type);
        this.pathSource = pathSource;
        this.metamodel = metamodel;
    }

    public <Y> Path<Y> getAttr(String attributeName) throws IllegalArgumentException {
        Attribute attribute = metamodel.entity(type).getAttribute(attributeName);
        Path<Y> newPathSource = new PathImpl<Y>(this.metamodel,new ExpressionAttributeImpl(type,  this.pathSource, this.metamodel, attribute),null);
        this.pathSource = (AbstractPathExpression) newPathSource;
        return newPathSource;
    }

    public <Y> Path<Y> getAttr(SingularAttribute<? super X, Y> attribute) {
        this.pathSource = new ExpressionAttributeImpl(type,  this.pathSource, this.metamodel, attribute);
        return this.pathSource;
    }


    public Path<?> getParentPath() {
        return this.pathSource;
    }

}



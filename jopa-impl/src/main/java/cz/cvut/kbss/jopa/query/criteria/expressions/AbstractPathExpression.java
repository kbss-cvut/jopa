package cz.cvut.kbss.jopa.query.criteria.expressions;

import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.model.metamodel.SingularAttribute;
import cz.cvut.kbss.jopa.model.query.criteria.Path;
import cz.cvut.kbss.jopa.query.criteria.PathImpl;
import cz.cvut.kbss.jopa.sessions.CriteriaBuilder;

abstract public class AbstractPathExpression<X> extends AbstractExpression<X> implements Path<X> {

    protected AbstractPathExpression pathSource;
    protected final Metamodel metamodel;

    public AbstractPathExpression(Class<X> type, AbstractPathExpression pathSource, Metamodel metamodel, CriteriaBuilder cb) {
        super(type, cb);
        this.pathSource = pathSource;
        this.metamodel = metamodel;
    }

    public <Y> Path<Y> getAttr(String attributeName) throws IllegalArgumentException {
        Attribute attribute = metamodel.entity(type).getAttribute(attributeName);
        Path<Y> newPathSource = new PathImpl<Y>(this.metamodel,new ExpressionAttributeImpl(attribute.getJavaType(),  this.pathSource, this.metamodel, attribute,this.cb),attribute.getJavaType(),this.cb);
        return newPathSource;
    }

    public <Y> Path<Y> getAttr(SingularAttribute<? super X, Y> attribute) {
        Path<Y> newPathSource = new PathImpl<Y>(this.metamodel, new ExpressionAttributeImpl(attribute.getJavaType(),  this.pathSource, this.metamodel, attribute, this.cb),attribute.getJavaType(),this.cb);
        return newPathSource;
    }


    public Path<?> getParentPath() {
        return this.pathSource;
    }

}



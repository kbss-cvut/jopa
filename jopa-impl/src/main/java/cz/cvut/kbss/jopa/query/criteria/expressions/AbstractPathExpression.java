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
        return new PathImpl<Y>(this.metamodel, this, attribute, this.cb);
    }

    public <Y> Path<Y> getAttr(SingularAttribute<? super X, Y> attribute) {
        return new PathImpl<>(this.metamodel, this, attribute, this.cb);
    }


    public Path<?> getParentPath() {
        return this.pathSource;
    }

}



package cz.cvut.kbss.jopa.query.criteria;

import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.model.query.criteria.Root;
import cz.cvut.kbss.jopa.query.criteria.expressions.AbstractPathExpression;
import cz.cvut.kbss.jopa.sessions.CriteriaBuilder;

public class RootImpl<X> extends PathImpl<X> implements Root<X> {

    public RootImpl(Metamodel metamodel, AbstractPathExpression<X> expression, Class<X> type, CriteriaBuilder cb) {
        super(metamodel, expression, type, cb);
    }

    @Override
    public EntityType<X> getModel() {
        return metamodel.entity(type);
    }
}

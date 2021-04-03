package cz.cvut.kbss.jopa.query.criteria;

import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.model.query.criteria.Root;
import cz.cvut.kbss.jopa.query.criteria.expressions.AbstractPathExpression;

public class RootImpl<X> extends PathImpl<X> implements Root<X> {

    public RootImpl(Metamodel metamodel, AbstractPathExpression<X> expression, Class<X> type) {
        super(metamodel, expression, type);
    }

    @Override
    public EntityType<X> getModel() {
        return metamodel.entity(type);
    }
}

package cz.cvut.kbss.jopa.query.criteria;

import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.model.metamodel.SingularAttribute;
import cz.cvut.kbss.jopa.model.query.criteria.Path;
import cz.cvut.kbss.jopa.model.query.criteria.Root;

public class RootImpl<X> extends PathImpl<X> implements Root<X> {

    public RootImpl(Metamodel metamodel, Class<X> type) {
        super(metamodel, type);
    }

    @Override
    public EntityType<X> getModel() {
        return metamodel.entity(type);
    }

    @Override
    public void setExpressionToQuery(StringBuilder query) {
        String className = getModel().getJavaType().getSimpleName();
        query.append(className + " " + className.toLowerCase());
    }
}

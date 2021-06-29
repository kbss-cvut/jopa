package cz.cvut.kbss.jopa.model.query.criteria;

import cz.cvut.kbss.jopa.model.metamodel.EntityType;

public interface Root<X> extends Path<X>{

    /**
     * Return the metamodel entity corresponding to the root.
     * @return metamodel entity corresponding to the root
     */
    EntityType<X> getModel();
}

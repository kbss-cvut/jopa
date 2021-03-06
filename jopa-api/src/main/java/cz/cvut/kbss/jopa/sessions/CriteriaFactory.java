package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.query.criteria.CriteriaQuery;
import cz.cvut.kbss.jopa.model.query.criteria.PredicateFactory;

public interface CriteriaFactory extends PredicateFactory {

    /**
     * Create a CriteriaQuery object.
     * @return criteria query object
     */
    CriteriaQuery<Object> createQuery();

    /**
     * Create a CriteriaQuery object with the specified result type.
     * @param resultClass type of the query result
     * @return criteria query object
     */
    <T> CriteriaQuery<T> createQuery(Class<T> resultClass);

}

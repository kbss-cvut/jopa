package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.model.query.criteria.CriteriaQuery;

//TODO PRO - CriteriaFactory methods
public interface CriteriaFactory {
    <X> CriteriaQuery<X> from(Class<X> entityClass);
}

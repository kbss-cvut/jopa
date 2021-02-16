package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.model.query.criteria.CriteriaQuery;
import cz.cvut.kbss.jopa.model.query.criteria.CriteriaQueryModel;

//TODO PRO - CriteriaFactory methods
public interface CriteriaFactory {

    /**
     * Return a criteriaQueryModel.
     * @return criteriaQueryModel
     */
    CriteriaQueryModel<?> getCriteriaQueryModel(Class<?> queryModelType);

    <X> CriteriaQuery<X> from(Class<X> entityClass);
}

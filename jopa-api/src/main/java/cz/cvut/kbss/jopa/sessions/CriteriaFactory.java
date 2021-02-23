package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.query.criteria.CriteriaQuery;
import cz.cvut.kbss.jopa.model.query.criteria.CriteriaModel;

//TODO PRO - CriteriaFactory methods
public interface CriteriaFactory {

    /**
     * Return a criteriaQueryModel.
     * @return criteriaQueryModel
     */
    <T> CriteriaModel<T> getCriteriaQueryModel(Class<T> queryModelType);

    <X> CriteriaQuery<X> from(Class<X> entityClass);

    <X> CriteriaQuery<X> from(EntityType<X> entity);
}

package cz.cvut.kbss.jopa.model.query.criteria;

//TODO PRO - CriteriaFactory methods
public interface CriteriaFactory<T> {
    <X>CriteriaQuery<X> from(Class<X> entityClass);
}

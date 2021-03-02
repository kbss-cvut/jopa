package cz.cvut.kbss.jopa.query.criteria;

import cz.cvut.kbss.jopa.model.CriteriaQueryImpl;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.query.criteria.CriteriaModel;
import cz.cvut.kbss.jopa.model.query.criteria.CriteriaQuery;
import cz.cvut.kbss.jopa.sessions.CriteriaFactory;
import cz.cvut.kbss.jopa.sessions.UnitOfWorkImpl;

public class CriteriaFactoryImpl implements CriteriaFactory {

    private final UnitOfWorkImpl uow;

    public CriteriaFactoryImpl(UnitOfWorkImpl uow) {
        this.uow = uow;
    }

    @Override
    public <T> CriteriaModel<T> getCriteriaQueryModel(Class<T> queryModelType) {
        EntityType<T> entityMetamodel = uow.getMetamodel().entity(queryModelType);
        return new CriteriaModelImpl<T>(entityMetamodel, queryModelType, uow);
    }

    @Override
    public <X> CriteriaQuery<X> from(Class<X> resultClass) {
        EntityType<X> entityMetamodel = uow.getMetamodel().entity(resultClass);
        return new CriteriaQueryImpl<X>(new CriteriaQueryHolder<X>(entityMetamodel, resultClass));
    }

    @Override
    public <X> CriteriaQuery<X> from(EntityType<X> entityMetamodel) {
        return new CriteriaQueryImpl<X>(new CriteriaQueryHolder<X>(entityMetamodel, entityMetamodel.getBindableJavaType()));
    }

}

package cz.cvut.kbss.jopa.oom.query;

import cz.cvut.kbss.jopa.exceptions.NoResultException;
import cz.cvut.kbss.jopa.exceptions.NoUniqueResultException;
import cz.cvut.kbss.jopa.model.metamodel.AbstractQueryAttribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.query.TypedQuery;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class SingularQueryAttributeStrategy<X> extends QueryFieldStrategy<AbstractQueryAttribute<? super X, ?>, X> {

    private static final Logger LOG = LoggerFactory.getLogger(SingularQueryAttributeStrategy.class);

    private Object value;

    public SingularQueryAttributeStrategy(EntityType<X> et, AbstractQueryAttribute<? super X, ?> attribute) {
        super(et, attribute);
    }

    @Override
    public void addValueFromTypedQuery(TypedQuery<?> typedQuery) {
        Object querySingleResult;
        try {
            querySingleResult = typedQuery.getSingleResult();
        } catch (NoResultException e1) {
            LOG.trace("Query \"{}\" for attribute {} did not return any result.", attribute.getQuery(), attribute);
            return;
        } catch (NoUniqueResultException e2) {
            LOG.trace("Query \"{}\" for attribute {} does not have a unique result.", attribute.getQuery(), attribute);
            return;
        }

        if (!isValidRange(querySingleResult)) {
            return;
        }

        this.value = toAttributeValue(querySingleResult);
    }

    @Override
    public void buildInstanceFieldValue(Object instance) throws IllegalAccessException {
        setValueOnInstance(instance, value);
    }
}

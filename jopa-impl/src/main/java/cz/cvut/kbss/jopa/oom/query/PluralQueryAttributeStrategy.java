package cz.cvut.kbss.jopa.oom.query;

import cz.cvut.kbss.jopa.model.metamodel.AbstractPluralQueryAttribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.query.TypedQuery;
import cz.cvut.kbss.jopa.utils.CollectionFactory;

import java.util.Collection;
import java.util.stream.Stream;

public class PluralQueryAttributeStrategy<X> extends QueryFieldStrategy<AbstractPluralQueryAttribute<? super X, ?, ?>, X> {

    private final Class<?> elementType;

    private final Collection<Object> values;

    public PluralQueryAttributeStrategy(EntityType<X> et, AbstractPluralQueryAttribute<? super X, ?, ?> attribute) {
        super(et, attribute);
        this.values = CollectionFactory.createDefaultCollection(attribute.getCollectionType());
        this.elementType = attribute.getElementType().getJavaType();
    }

    @Override
    public void addValueFromTypedQuery(TypedQuery<?> typedQuery) {
        Stream<?> queryResultList = typedQuery.getResultStream();
        //TODO
    }

    @Override
    public void buildInstanceFieldValue(Object instance) throws IllegalAccessException {
        if (! values.isEmpty()) {
            setValueOnInstance(instance, values);
        }
    }
}

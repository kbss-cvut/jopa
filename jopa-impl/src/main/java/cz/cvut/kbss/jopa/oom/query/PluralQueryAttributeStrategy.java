package cz.cvut.kbss.jopa.oom.query;

import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.PluralQueryAttributeImpl;
import cz.cvut.kbss.jopa.model.query.TypedQuery;
import cz.cvut.kbss.jopa.oom.converter.ToLexicalFormConverter;
import cz.cvut.kbss.jopa.utils.CollectionFactory;
import cz.cvut.kbss.jopa.utils.IdentifierTransformer;
import cz.cvut.kbss.ontodriver.model.NamedResource;

import java.util.Collection;

public class PluralQueryAttributeStrategy<X> extends QueryFieldStrategy<PluralQueryAttributeImpl<? super X, ?, ?>, X> {

    private final Class<?> elementType;

    private final Collection<Object> values;

    public PluralQueryAttributeStrategy(EntityType<X> et, PluralQueryAttributeImpl<? super X, ?, ?> attribute) {
        super(et, attribute);
        this.values = CollectionFactory.createDefaultCollection(attribute.getCollectionType());
        this.elementType = attribute.getElementType().getJavaType();
    }

    @Override
    public void addValueFromTypedQuery(TypedQuery<?> typedQuery) {
        typedQuery.getResultStream()
                .map(this::toAttributeValue)
                .forEach(values::add);
    }

    @Override
    public void buildInstanceFieldValue(Object instance) throws IllegalAccessException {
        if (! values.isEmpty()) {
            setValueOnInstance(instance, values);
        }
    }
}

package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.PluralAttribute;
import cz.cvut.kbss.jopa.utils.CollectionFactory;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.Value;

import java.util.Collection;
import java.util.Set;
import java.util.stream.Collectors;

public class PluralDataPropertyStrategy<X> extends DataPropertyFieldStrategy<X> {

    private PluralAttribute<? super X, ?, ?> pluralAtt;

    private Collection<Object> values;

    PluralDataPropertyStrategy(EntityType<X> et, PluralAttribute<? super X, ?, ?> att, Descriptor attributeDescriptor,
                               EntityMappingHelper mapper) {
        super(et, att, attributeDescriptor, mapper);
        this.pluralAtt = att;
        this.values = CollectionFactory.createDefaultCollection(pluralAtt.getCollectionType());
    }

    @Override
    void addValueFromAxiom(Axiom<?> ax) {
        final Object value = ax.getValue().getValue();
        if (isValidRange(value)) {
            this.values.add(value);
        }
    }

    boolean isValidRange(Object value) {
        return pluralAtt.getElementType().getJavaType().isAssignableFrom(value.getClass());
    }

    @Override
    void buildInstanceFieldValue(Object instance) throws IllegalAccessException {
        if (!values.isEmpty()) {
            setValueOnInstance(instance, values);
        }
    }

    @Override
    void buildAxiomValuesFromInstance(X instance, AxiomValueGatherer valueBuilder) throws IllegalAccessException {
        final Object value = extractFieldValueFromInstance(instance);
        assert value instanceof Collection || value == null;
        final Collection<?> valueCollection = (Collection<?>) value;
        if (valueCollection == null || valueCollection.isEmpty()) {
            valueBuilder.addValue(createAssertion(), Value.nullValue(), getAttributeContext());
        } else {
            final Set<Value<?>> assertionValues = valueCollection.stream().map(Value::new).collect(Collectors.toSet());
            valueBuilder.addValues(createAssertion(), assertionValues, getAttributeContext());
        }
    }
}

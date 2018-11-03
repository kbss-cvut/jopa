package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.AbstractPluralAttribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.utils.IdentifierTransformer;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;

import java.util.Collection;
import java.util.Objects;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

class PluralAnnotationPropertyStrategy<X> extends PluralDataPropertyStrategy<X> {

    PluralAnnotationPropertyStrategy(EntityType<X> et, AbstractPluralAttribute<? super X, ?, ?> att,
                                     Descriptor attributeDescriptor, EntityMappingHelper mapper) {
        super(et, att, attributeDescriptor, mapper);
    }

    @Override
    void addValueFromAxiom(Axiom<?> ax) {
        final Object value = ax.getValue().getValue();
        if (isValidRange(value)) {
            values.add(toAttributeValue(value));
        } else if (value instanceof NamedResource && IdentifierTransformer.isValidIdentifierType(elementType)) {
            values.add(IdentifierTransformer.transformToIdentifier(value, elementType));
        }
    }

    @Override
    void buildAxiomValuesFromInstance(X instance, AxiomValueGatherer valueBuilder) {
        final Object value = extractFieldValueFromInstance(instance);
        assert value instanceof Collection || value == null;
        final Collection<?> valueCollection = (Collection<?>) value;
        if (valueCollection == null || valueCollection.isEmpty()) {
            valueBuilder.addValue(createAssertion(), Value.nullValue(), getAttributeContext());
        } else {
            final Function<Object, Value<?>> mapper;
            if (IdentifierTransformer.isValidIdentifierType(elementType) && !elementType
                    .isAssignableFrom(String.class)) {
                mapper = v -> new Value<>(NamedResource.create(IdentifierTransformer.valueAsUri(v)));
            } else {
                mapper = v -> new Value<>(toAxiomValue(v));
            }
            final Set<Value<?>> assertionValues =
                    valueCollection.stream().filter(Objects::nonNull).map(mapper).collect(Collectors.toSet());
            valueBuilder.addValues(createAssertion(), assertionValues, getAttributeContext());
        }
    }

    @Override
    Assertion createAssertion() {
        return Assertion
                .createAnnotationPropertyAssertion(attribute.getIRI().toURI(), getLanguage(), attribute.isInferred());
    }
}

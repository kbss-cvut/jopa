package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.model.MultilingualString;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.ListAttributeImpl;
import cz.cvut.kbss.jopa.oom.converter.ToLexicalFormConverter;
import cz.cvut.kbss.jopa.utils.IdentifierTransformer;
import cz.cvut.kbss.jopa.utils.MultilingualStringCondenser;
import cz.cvut.kbss.ontodriver.descriptor.ReferencedListValueDescriptor;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.LangString;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;

import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.function.Function;

public class ReferencedListAnnotationPropertyStrategy<X> extends ReferencedListDataPropertyStrategy<X> {

    final MultilingualStringCondenser multilingualStringCondenser = new MultilingualStringCondenser();

    ReferencedListAnnotationPropertyStrategy(EntityType<X> et, ListAttributeImpl<? super X, ?> att,
                                             Descriptor attributeDescriptor, EntityMappingHelper mapper) {
        super(et, att, attributeDescriptor, mapper);
    }

    @Override
    void handleSingleElementAxiom(Axiom<?> item) {
        final Object value = item.getValue().getValue();
        if (isValidRange(value)) {
            if (value instanceof LangString ls && elementType.isAssignableFrom(MultilingualString.class)) {
                multilingualStringCondenser.add(ls);
            } else {
                values.add(toAttributeValue(value));
            }
        } else if (value instanceof NamedResource && IdentifierTransformer.isValidIdentifierType(elementType)) {
            values.add(IdentifierTransformer
                    .transformToIdentifier(ToLexicalFormConverter.INSTANCE.convertToAttribute(value),
                            elementType));
        }
    }

    @Override
    void buildInstanceFieldValue(Object instance) {
        values.addAll(multilingualStringCondenser.getValues());
        super.buildInstanceFieldValue(instance);
    }

    @Override
    void buildAxiomValuesFromInstance(X instance, AxiomValueGatherer valueBuilder) {
        final Object value = extractFieldValueFromInstance(instance);
        assert value instanceof List || value == null;

        final ReferencedListValueDescriptor<Object> listDescriptor = createListValueDescriptor(instance);
        final List<?> list = (List<?>) value;
        if (list != null) {
            final Function<Object, List<?>> mapper = resolveValueMapper();
            list.stream().filter(Objects::nonNull)
                .flatMap(v -> mapper.apply(v).stream())
                .forEach(listDescriptor::addValue);
        }
        valueBuilder.addReferencedListValues(listDescriptor);
    }

    private Function<Object, List<?>> resolveValueMapper() {
        if (SingularAnnotationPropertyStrategy.isResourceIdentifierType(elementType)) {
            return v -> List.of(NamedResource.create(IdentifierTransformer.valueAsUri(v)));
        } else {
            return v -> this.toAxiomValue(v).stream().map(Value::getValue).toList();
        }
    }

    @Override
    Assertion createAssertion() {
        return Assertion
                .createAnnotationPropertyAssertion(attribute.getIRI().toURI(), getLanguage(), attribute.isInferred());
    }
}

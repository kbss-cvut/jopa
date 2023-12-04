package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.ListAttributeImpl;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;
import cz.cvut.kbss.ontodriver.descriptor.ReferencedListDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.ReferencedListDescriptorImpl;
import cz.cvut.kbss.ontodriver.descriptor.ReferencedListValueDescriptor;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Objects;
import java.util.Set;

class ReferencedListDataPropertyStrategy<X> extends DataPropertyFieldStrategy<ListAttributeImpl<? super X, ?>, X> {

    private final Class<?> elementType;

    private final List<Object> values = new ArrayList<>();

    ReferencedListDataPropertyStrategy(EntityType<X> et, ListAttributeImpl<? super X, ?> att,
                                       Descriptor attributeDescriptor, EntityMappingHelper mapper) {
        super(et, att, attributeDescriptor, mapper);
        this.elementType = att.getElementType().getJavaType();
    }

    @Override
    void addValueFromAxiom(Axiom<?> ax) {
        final ReferencedListDescriptor listDescriptor = createListDescriptor(ax);
        final Collection<Axiom<?>> sequence = mapper.loadReferencedList(listDescriptor);
        sequence.stream()
                .filter(a -> a.getAssertion().getIdentifier().equals(attribute.getOWLPropertyHasContentsIRI().toURI()))
                .forEach(a -> {
                    final Object value = ax.getValue().getValue();
                    if (isValidRange(value)) {
                        values.add(toAttributeValue(value));
                    }
                });
    }

    ReferencedListDescriptor createListDescriptor(Axiom<?> ax) {
        final NamedResource owner = ax.getSubject();

        final boolean inferred = attribute.isInferred();
        final Assertion listProperty = Assertion.createObjectPropertyAssertion(attribute.getIRI().toURI(), inferred);
        final Assertion nextNodeProperty = Assertion
                .createObjectPropertyAssertion(attribute.getOWLObjectPropertyHasNextIRI().toURI(), inferred);
        final Assertion nodeContentProperty = Assertion.createDataPropertyAssertion(attribute.getOWLPropertyHasContentsIRI()
                                                                                             .toURI(), inferred);
        final ReferencedListDescriptor listDescriptor = new ReferencedListDescriptorImpl(owner, listProperty,
                nextNodeProperty, nodeContentProperty);
        listDescriptor.setContext(getAttributeWriteContext());
        return listDescriptor;
    }

    @Override
    boolean isValidRange(Object value) {
        return elementType.isAssignableFrom(value.getClass()) || canBeConverted(value);
    }

    @Override
    void buildInstanceFieldValue(Object instance) {
        if (!values.isEmpty()) {
            setValueOnInstance(instance, values);
        }
    }

    @Override
    void buildAxiomValuesFromInstance(X instance, AxiomValueGatherer valueBuilder) {
        final Object value = extractFieldValueFromInstance(instance);
        assert value instanceof List || value == null;

        final ReferencedListValueDescriptor<Object> listDescriptor = createListValueDescriptor(instance);
        final List<?> list = (List<?>) value;
        if (list != null) {
            list.stream().filter(Objects::nonNull)
                .forEach(v -> listDescriptor.addValue(converter.convertToAxiomValue(value)));
        }
        valueBuilder.addReferencedListValues(listDescriptor);
    }

    private <V> ReferencedListValueDescriptor<V> createListValueDescriptor(X instance) {
        final URI owner = EntityPropertiesUtils.getIdentifier(instance, et);
        final boolean inferred = attribute.isInferred();
        final Assertion hasList = Assertion
                .createObjectPropertyAssertion(attribute.getIRI().toURI(), inferred);
        final Assertion hasNext = Assertion.createObjectPropertyAssertion(attribute
                .getOWLObjectPropertyHasNextIRI().toURI(), inferred);
        final Assertion hasContent = Assertion.createDataPropertyAssertion(attribute.getOWLPropertyHasContentsIRI()
                                                                                    .toURI(), inferred);
        final ReferencedListValueDescriptor<V> descriptor = new ReferencedListValueDescriptor<>(
                NamedResource.create(owner), hasList, hasNext, hasContent);
        descriptor.setContext(getAttributeWriteContext());
        return descriptor;
    }

    @Override
    Set<Axiom<?>> buildAxiomsFromInstance(X instance) {
        throw new UnsupportedOperationException("Method not supported for referenced lists.");
    }
}

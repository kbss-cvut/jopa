/*
 * JOPA
 * Copyright (C) 2024 Czech Technical University in Prague
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */
package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.ListAttributeImpl;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;
import cz.cvut.kbss.ontodriver.descriptor.ReferencedListDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.ReferencedListValueDescriptor;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;

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
    void addAxiomValue(Axiom<?> head) {
        final ReferencedListDescriptor listDescriptor = createListDescriptor(head);
        final Collection<Axiom<?>> sequence = mapper.loadReferencedList(listDescriptor);
        sequence.stream()
                .filter(item -> item.getAssertion().getIdentifier()
                                    .equals(attribute.getHasContentsPropertyIRI().toURI()))
                .forEach(item -> {
                    final Object value = item.getValue().getValue();
                    if (isValidRange(value)) {
                        values.add(toAttributeValue(value));
                    }
                });
    }

    ReferencedListDescriptor createListDescriptor(Axiom<?> ax) {
        final NamedResource owner = ax.getSubject();
        final ReferencedListDescriptor listDescriptor = ListDescriptorFactory.createReferencedListDescriptor(owner, attribute);
        listDescriptor.setContext(getAttributeWriteContext());
        return listDescriptor;
    }

    @Override
    boolean hasValue() {
        return !values.isEmpty();
    }

    @Override
    boolean isValidRange(Object value) {
        return elementType.isAssignableFrom(value.getClass()) || canBeConverted(value);
    }

    @Override
    void buildInstanceFieldValue(Object instance) {
        setValueOnInstance(instance, values);
    }

    @Override
    void buildAxiomValuesFromInstance(X instance, AxiomValueGatherer valueBuilder) {
        final Object value = extractFieldValueFromInstance(instance);
        assert value instanceof List || value == null;

        final ReferencedListValueDescriptor<Object> listDescriptor = createListValueDescriptor(instance);
        final List<?> list = (List<?>) value;
        if (list != null) {
            list.stream().filter(Objects::nonNull)
                .forEach(v -> listDescriptor.addValue(converter.convertToAxiomValue(v)));
        }
        valueBuilder.addReferencedListValues(listDescriptor);
    }

    private <V> ReferencedListValueDescriptor<V> createListValueDescriptor(X instance) {
        final NamedResource owner = NamedResource.create(EntityPropertiesUtils.getIdentifier(instance, et));
        final ReferencedListValueDescriptor<V> descriptor = ListDescriptorFactory.createReferencedListValueDescriptor(owner, attribute);
        descriptor.setContext(getAttributeWriteContext());
        return descriptor;
    }

    @Override
    Set<Axiom<?>> buildAxiomsFromInstance(X instance) {
        throw new UnsupportedOperationException("Method not supported for referenced lists.");
    }
}

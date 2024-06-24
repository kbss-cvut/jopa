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
import cz.cvut.kbss.jopa.model.metamodel.AbstractPluralAttribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.utils.CollectionFactory;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;
import cz.cvut.kbss.ontodriver.model.*;

import java.util.Collection;
import java.util.Collections;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

class PluralDataPropertyStrategy<X> extends DataPropertyFieldStrategy<AbstractPluralAttribute<? super X, ?, ?>, X> {

    final Class<?> elementType;

    final Collection<Object> values;

    PluralDataPropertyStrategy(EntityType<X> et, AbstractPluralAttribute<? super X, ?, ?> att,
                               Descriptor attributeDescriptor, EntityMappingHelper mapper) {
        super(et, att, attributeDescriptor, mapper);
        this.values = CollectionFactory.createDefaultCollection(att.getCollectionType());
        this.elementType = att.getElementType().getJavaType();
    }

    @Override
    void addAxiomValue(Axiom<?> ax) {
        final Object value = ax.getValue().getValue();
        if (isValidRange(value)) {
            this.values.add(toAttributeValue(value));
        }
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
        assert value instanceof Collection || value == null;
        final Collection<?> valueCollection = (Collection<?>) value;
        if (valueCollection == null || valueCollection.isEmpty()) {
            valueBuilder.addValue(createAssertion(), Value.nullValue(), getAttributeWriteContext());
        } else {
            final Set<Value<?>> assertionValues = valueCollection.stream()
                                                                 .filter(Objects::nonNull)
                                                                 .map(this::convertToAxiomValue)
                                                                 .collect(Collectors.toSet());
            valueBuilder.addValues(createAssertion(),
                    filterOutInferredValues(valueBuilder.getSubjectIdentifier(), assertionValues),
                    getAttributeWriteContext());
        }
    }

    @Override
    Set<Axiom<?>> buildAxiomsFromInstance(X instance) {
        final Object value = extractFieldValueFromInstance(instance);
        assert value instanceof Collection || value == null;
        final Collection<?> valueCollection = (Collection<?>) value;
        if (valueCollection == null || valueCollection.isEmpty()) {
            return Collections.emptySet();
        } else {
            final NamedResource subject = NamedResource.create(EntityPropertiesUtils.getIdentifier(instance, et));
            final Assertion assertion = createAssertion();
            return valueCollection.stream().filter(Objects::nonNull)
                                  .map(v -> new AxiomImpl<>(subject, assertion, convertToAxiomValue(v)))
                                  .collect(Collectors.toSet());
        }
    }
}

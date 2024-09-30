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
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;
import cz.cvut.kbss.jopa.utils.IdentifierTransformer;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;

import java.net.URI;
import java.util.Collection;
import java.util.HashSet;
import java.util.Objects;
import java.util.Set;

class SimpleSetPropertyStrategy<X> extends PluralObjectPropertyStrategy<AbstractPluralAttribute<? super X, ?, ?>, X> {

    SimpleSetPropertyStrategy(EntityType<X> et, AbstractPluralAttribute<? super X, ?, ?> att, Descriptor descriptor,
                              EntityMappingHelper mapper) {
        super(et, att, descriptor, mapper);
    }

    @Override
    void buildAxiomValuesFromInstance(X instance, AxiomValueGatherer valueBuilder) {
        final Object value = extractFieldValueFromInstance(instance);
        assert value instanceof Collection || value == null;
        final Collection<?> valueCollection = (Collection<?>) value;
        extractValues(valueCollection, valueBuilder);
    }

    private <T> void extractValues(Collection<T> valueCollection, AxiomValueGatherer valueBuilder) {
        if (valueCollection == null) {
            valueBuilder.addValue(createAssertion(), Value.nullValue(), getAttributeWriteContext());
            return;
        }
        final Class<?> elemType = attribute.getBindableJavaType();
        final Set<Value<?>> assertionValues = new HashSet<>(valueCollection.size());
        if (IdentifierTransformer.isValidIdentifierType(elemType)) {
            valueCollection.stream().filter(Objects::nonNull).forEach(item -> assertionValues
                    .add(new Value<>(NamedResource.create(IdentifierTransformer.valueAsUri(item)))));
        } else if (elemType.isEnum()) {
            assert attribute.getConverter() != null;
            valueCollection.stream().filter(Objects::nonNull).forEach(
                    item -> assertionValues.add(new Value<>(attribute.getConverter().convertToAxiomValue(item))));
        } else {
            final EntityType<T> et = (EntityType<T>) mapper.getEntityType(elemType);
            valueCollection.stream().filter(Objects::nonNull).forEach(item -> {
                if (referenceSavingResolver.shouldSaveReferenceToItem(item, getAttributeValueContexts())) {
                    final URI valId = EntityPropertiesUtils.getIdentifier(item, et);
                    assert valId != null;
                    assertionValues.add(new Value<>(NamedResource.create(valId)));
                } else {
                    referenceSavingResolver
                            .registerPendingReference(valueBuilder.getSubjectIdentifier(), createAssertion(), item,
                                    getAttributeWriteContext());
                }
            });
        }
        valueBuilder.addValues(createAssertion(),
                filterOutInferredValues(valueBuilder.getSubjectIdentifier(), assertionValues),
                getAttributeWriteContext());
    }
}

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

import cz.cvut.kbss.jopa.model.MultilingualString;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.AbstractPluralAttribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.oom.converter.ToLexicalFormConverter;
import cz.cvut.kbss.jopa.utils.IdentifierTransformer;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;

import java.util.Collection;
import java.util.Collections;
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
    void addAxiomValue(Axiom<?> ax) {
        final Object value = ax.getValue().getValue();
        if (isValidRange(value)) {
            values.add(toAttributeValue(value));
        } else if (value instanceof NamedResource && IdentifierTransformer.isValidIdentifierType(elementType)) {
            values.add(IdentifierTransformer
                    .transformToIdentifier(ToLexicalFormConverter.INSTANCE.convertToAttribute(value),
                            elementType));
        }
    }

    @Override
    void buildAxiomValuesFromInstance(X instance, AxiomValueGatherer valueBuilder) {
        final Object value = extractFieldValueFromInstance(instance);
        assert value instanceof Collection || value == null;
        final Collection<?> valueCollection = (Collection<?>) value;
        if (valueCollection == null || valueCollection.isEmpty()) {
            valueBuilder.addValue(createAssertion(), Value.nullValue(), getAttributeWriteContext());
        } else {
            final Function<Object, Collection<Value<?>>> mapper = resolveValueMapper();
            final Set<Value<?>> assertionValues =
                    valueCollection.stream().filter(Objects::nonNull).map(mapper).flatMap(Collection::stream)
                                   .collect(Collectors.toSet());
            valueBuilder.addValues(createAssertion(),
                    filterOutInferredValues(valueBuilder.getSubjectIdentifier(), assertionValues),
                    getAttributeWriteContext());
        }
    }

    private Function<Object, Collection<Value<?>>> resolveValueMapper() {
        if (SingularAnnotationPropertyStrategy.isResourceIdentifierType(elementType)) {
            return  v -> Collections.singleton(new Value<>(NamedResource.create(IdentifierTransformer.valueAsUri(v))));
        } else {
            return v -> v instanceof MultilingualString ?
                    SingularMultilingualStringFieldStrategy.translationsToLangStrings(
                            (MultilingualString) v).collect(Collectors.toList()) : Collections.singleton(convertToAxiomValue(v));
        }
    }

    @Override
    Collection<Value<?>> toAxiomValue(Object value) {
        return resolveValueMapper().apply(value);
    }

    @Override
    Assertion createAssertion() {
        return Assertion
                .createAnnotationPropertyAssertion(attribute.getIRI().toURI(), getLanguage(), attribute.isInferred());
    }
}

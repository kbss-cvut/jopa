/*
 * JOPA
 * Copyright (C) 2023 Czech Technical University in Prague
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

import cz.cvut.kbss.jopa.exceptions.CardinalityConstraintViolatedException;
import cz.cvut.kbss.jopa.model.MultilingualString;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.AbstractAttribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;
import cz.cvut.kbss.ontodriver.model.*;

import java.util.Collection;
import java.util.Collections;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

class SingularMultilingualStringFieldStrategy<X>
        extends DataPropertyFieldStrategy<AbstractAttribute<? super X, MultilingualString>, X> {

    private MultilingualString value;

    SingularMultilingualStringFieldStrategy(EntityType<X> et, AbstractAttribute<? super X, MultilingualString> att,
                                            Descriptor entityDescriptor, EntityMappingHelper mapper) {
        super(et, att, entityDescriptor, mapper);
    }

    @Override
    void addAxiomValue(Axiom<?> ax) {
        if (value == null) {
            this.value = new MultilingualString();
        }
        final Value<?> axiomValue = ax.getValue();
        if (axiomValue.getValue() instanceof LangString lsAxiomValue) {
            verifyCardinality(lsAxiomValue.getLanguage().orElse(null), ax.getSubject());
            value.set(lsAxiomValue.getLanguage().orElse(null), lsAxiomValue.getValue());
        } else {
            value.set(axiomValue.stringValue());
        }
    }

    private void verifyCardinality(String language, NamedResource subject) {
        if (language != null && value.contains(language)) {
            throw new CardinalityConstraintViolatedException(String.format(
                    "Expected single value with language tag %s of attribute %s of instance <%s>, but got multiple.",
                    language, attribute.getName(), subject));
        } else if (language == null && value.containsSimple()) {
            throw new CardinalityConstraintViolatedException(String.format(
                    "Expected single simple literal value of attribute %s of instance <%s>, but got multiple.",
                    attribute.getName(), subject));
        }
    }

    @Override
    boolean hasValue() {
        return value != null;
    }

    @Override
    void buildInstanceFieldValue(Object instance) {
        setValueOnInstance(instance, value);
    }

    @Override
    void buildAxiomValuesFromInstance(X instance, AxiomValueGatherer valueBuilder) {
        final MultilingualString attValue = (MultilingualString) extractFieldValueFromInstance(instance);
        if (attValue == null || attValue.isEmpty()) {
            valueBuilder.addValue(createAssertion(), Value.nullValue(), getAttributeWriteContext());
        } else {
            valueBuilder.addValues(createAssertion(), translationsToLangStrings(attValue).collect(Collectors.toList()), getAttributeWriteContext());
        }
    }

    static Stream<Value<?>> translationsToLangStrings(MultilingualString str) {
        return str.getValue().entrySet().stream().map(e -> new Value<>(new LangString(e.getValue(), e.getKey())));
    }

    @Override
    Set<Axiom<?>> buildAxiomsFromInstance(X instance) {
        final MultilingualString attValue = (MultilingualString) extractFieldValueFromInstance(instance);
        if (attValue == null || attValue.isEmpty()) {
            return Collections.emptySet();
        } else {
            final NamedResource id = NamedResource.create(EntityPropertiesUtils.getIdentifier(instance, et));
            final Assertion assertion = createAssertion();
            return translationsToLangStrings(attValue).map(v -> new AxiomImpl<>(id, assertion, v)).collect(
                    Collectors.toSet());
        }
    }

    @Override
    Collection<Value<?>> toAxiomValue(Object value) {
        assert value instanceof MultilingualString;
        return translationsToLangStrings((MultilingualString) value).collect(Collectors.toList());
    }
}

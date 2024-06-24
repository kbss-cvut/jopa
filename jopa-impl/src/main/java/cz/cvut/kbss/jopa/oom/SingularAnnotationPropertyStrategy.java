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
import cz.cvut.kbss.jopa.model.metamodel.AbstractAttribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.oom.converter.ToLexicalFormConverter;
import cz.cvut.kbss.jopa.utils.IdentifierTransformer;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;

import java.net.URI;
import java.net.URL;
import java.util.stream.Collectors;

class SingularAnnotationPropertyStrategy<X> extends SingularDataPropertyStrategy<X> {

    SingularAnnotationPropertyStrategy(EntityType<X> et, AbstractAttribute<? super X, ?> att, Descriptor descriptor,
                                       EntityMappingHelper mapper) {
        super(et, att, descriptor, mapper);
    }

    @Override
    void addAxiomValue(Axiom<?> ax) {
        final Object val = ax.getValue().getValue();
        if (!isValidRange(val)) {
            return;
        }
        verifyCardinalityConstraint(ax.getSubject());
        if (IdentifierTransformer.isValidIdentifierType(attribute.getJavaType())) {
            this.value = IdentifierTransformer
                    .transformToIdentifier(ToLexicalFormConverter.INSTANCE.convertToAttribute(val),
                                           attribute.getJavaType());
        } else {
            this.value = toAttributeValue(val);
        }
    }

    @Override
    boolean isValidRange(Object value) {
        assert value != null;

        return value instanceof NamedResource && IdentifierTransformer.isValidIdentifierType(attribute.getJavaType()) ||
                super.isValidRange(value);
    }

    @Override
    void buildAxiomValuesFromInstance(X instance, AxiomValueGatherer valueBuilder) {
        final Object value = extractFieldValueFromInstance(instance);
        if (value == null) {
            valueBuilder.addValue(createAssertion(), Value.nullValue(), getAttributeWriteContext());
            return;
        }

        if (isResourceIdentifierType(attribute.getJavaType())) {
            valueBuilder.addValue(createAssertion(),
                                  new Value<>(NamedResource.create(IdentifierTransformer.valueAsUri(value))),
                                  getAttributeWriteContext());
        } else if (value instanceof MultilingualString) {
            valueBuilder.addValues(createAssertion(),
                                   SingularMultilingualStringFieldStrategy.translationsToLangStrings(
                                                                                  (MultilingualString) value)
                                                                          .collect(Collectors.toList()),
                                   getAttributeWriteContext());
        } else {
            valueBuilder.addValue(createAssertion(), convertToAxiomValue(value), getAttributeWriteContext());
        }
    }

    @Override
    Assertion createAssertion() {
        return Assertion
                .createAnnotationPropertyAssertion(attribute.getIRI().toURI(), getLanguage(), attribute.isInferred());
    }

    /**
     * Checks whether the specified class can be used to represent a resource identifier in an annotation property
     * value.
     * <p>
     * This means that only instances of types for which this method returns {@code true} are treated as resource
     * identifiers when mapping to the underlying repository.
     *
     * @param cls Type to check
     * @return {@code true} if the specified type represents resource identifier w.r.t. annotation properties, {@code
     * false} otherwise
     */
    static boolean isResourceIdentifierType(Class<?> cls) {
        return URI.class.equals(cls) || URL.class.equals(cls);
    }
}

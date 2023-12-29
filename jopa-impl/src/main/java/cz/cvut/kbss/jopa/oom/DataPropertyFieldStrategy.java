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

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.AbstractAttribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.oom.converter.ConverterWrapper;
import cz.cvut.kbss.jopa.oom.converter.DefaultConverterWrapper;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Value;

import java.util.Collection;
import java.util.Collections;

abstract class DataPropertyFieldStrategy<A extends AbstractAttribute<? super X, ?>, X> extends FieldStrategy<A, X> {

    protected final ConverterWrapper<Object, Object> converter;

    DataPropertyFieldStrategy(EntityType<X> et, A att, Descriptor entityDescriptor,
                              EntityMappingHelper mapper) {
        super(et, att, entityDescriptor, mapper);
        this.converter = att.getConverter() != null ? att.getConverter() : DefaultConverterWrapper.INSTANCE;
    }

    boolean isValidRange(Object value) {
        return attribute.getJavaType().isAssignableFrom(value.getClass()) || canBeConverted(value);
    }

    boolean canBeConverted(Object value) {
        return converter.supportsAxiomValueType(value.getClass());
    }

    Object toAttributeValue(Object value) {
        assert value != null;
        return converter.convertToAttribute(value);
    }

    Value<?> convertToAxiomValue(Object value) {
        assert value != null;
        return new Value<>(converter.convertToAxiomValue(value));
    }

    @Override
    Collection<Value<?>> toAxiomValue(Object value) {
        return Collections.singleton(value != null ? convertToAxiomValue(value) : Value.nullValue());
    }

    String getLanguage() {
        if (attribute.isSimpleLiteral()) {
            return null;
        }
        final Descriptor attDescriptor = entityDescriptor.getAttributeDescriptor(attribute);
        return attDescriptor.hasLanguage() ? attDescriptor.getLanguage() : attribute.getLanguage();
    }

    @Override
    Assertion createAssertion() {
        return Assertion.createDataPropertyAssertion(attribute.getIRI().toURI(), getLanguage(), attribute.isInferred());
    }
}

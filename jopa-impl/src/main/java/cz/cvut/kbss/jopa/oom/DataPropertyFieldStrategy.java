/**
 * Copyright (C) 2016 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License along with this program. If not, see
 * <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.utils.DatatypeTransformer;
import cz.cvut.kbss.ontodriver.model.Assertion;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Date;

abstract class DataPropertyFieldStrategy<X> extends FieldStrategy<Attribute<? super X, ?>, X> {

    final ValueResolver valueResolver;

    DataPropertyFieldStrategy(EntityType<X> et, Attribute<? super X, ?> att, Descriptor attributeDescriptor,
                              EntityMappingHelper mapper) {
        super(et, att, attributeDescriptor, mapper);
        this.valueResolver = getValueResolver();
    }

    private ValueResolver getValueResolver() {
        if (attribute.getJavaType().equals(LocalDate.class)) {
            return new LocalDateValueResolver();
        } else if (attribute.getJavaType().equals(LocalDateTime.class)) {
            return new LocalDateTimeValueResolver();
        } else {
            return new ValueResolver();
        }
    }

    boolean isValidRange(Object value) {
        return attribute.getJavaType().isAssignableFrom(value.getClass()) || isFieldEnum() || canBeTransformed(value);
    }

    boolean isFieldEnum() {
        final Class<?> cls = attribute.getJavaField().getType();
        return cls.isEnum();
    }

    private boolean canBeTransformed(Object value) {
        return (attribute.getJavaType().equals(LocalDate.class) ||
                attribute.getJavaType().equals(LocalDateTime.class)) && value instanceof Date;
    }

    Object resolveEnumValue(Object value) {
        final Class cls = attribute.getJavaField().getType();
        return Enum.valueOf(cls, value.toString());
    }

    @Override
    Assertion createAssertion() {
        return Assertion.createDataPropertyAssertion(attribute.getIRI().toURI(), getLanguage(), attribute.isInferred());
    }

    static class ValueResolver {

        Object toAxiomValue(Object value) {
            return value;
        }

        Object fromAxiom(Object axiomValue) {
            return axiomValue;
        }
    }

    static class LocalDateValueResolver extends ValueResolver {

        @Override
        Object toAxiomValue(Object value) {
            return DatatypeTransformer.transform(value, Date.class);
        }

        @Override
        Object fromAxiom(Object axiomValue) {
            return DatatypeTransformer.transform(axiomValue, LocalDate.class);
        }
    }

    static class LocalDateTimeValueResolver extends LocalDateValueResolver {

        @Override
        Object fromAxiom(Object axiomValue) {
            return DatatypeTransformer.transform(axiomValue, LocalDateTime.class);
        }
    }
}

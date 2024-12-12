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
package cz.cvut.kbss.jopa.sessions.validator;

import cz.cvut.kbss.jopa.exceptions.CardinalityConstraintViolatedException;
import cz.cvut.kbss.jopa.model.annotations.ParticipationConstraint;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.model.metamodel.QueryAttribute;
import cz.cvut.kbss.jopa.proxy.lazy.LazyLoadingProxy;

import java.lang.reflect.Field;
import java.util.Collection;

/**
 * Validator of participation constraints.
 */
class CardinalityConstraintsValidator extends IntegrityConstraintsValidator {

    CardinalityConstraintsValidator() {
    }

    @Override
    public void validate(Object identifier, FieldSpecification<?, ?> attribute, Object attributeValue) {
        final int valueCount = extractValueCount(attributeValue);

        // Only proper attributes can have cardinality constraints
        if (attribute instanceof Attribute<?, ?> att) {
            for (ParticipationConstraint pc : att.getConstraints()) {
                validateParticipationConstraint(identifier, att.getJavaField(), valueCount, pc);
            }
            if (att.getConstraints().length == 0) {
                validateNonEmpty(identifier, att, valueCount);
            }
        } else if (attribute instanceof QueryAttribute<?, ?> queryAtt) {
            for (ParticipationConstraint pc : queryAtt.getConstraints()) {
                validateParticipationConstraint(identifier, queryAtt.getJavaField(), valueCount, pc);
            }
        }
    }

    private static int extractValueCount(Object value) {
        if (value == null || value instanceof LazyLoadingProxy<?>) {
            return 0;
        }
        return value instanceof Collection ? ((Collection<?>) value).size() : 1;
    }

    private static void validateParticipationConstraint(Object id, Field field, int valueCount, ParticipationConstraint pc) {
        if (valueCount < pc.min()) {
            throw new CardinalityConstraintViolatedException("At least " + pc.min() +
                    " values of attribute " + field.getDeclaringClass().getSimpleName() + "." + field.getName() +
                    " expected in instance " + id + ", but got only " + valueCount);
        }
        if (pc.max() >= 0 && pc.max() < valueCount) {
            throw new CardinalityConstraintViolatedException("At most " + pc.max() +
                    " values of attribute " + field.getDeclaringClass().getSimpleName() + "." + field.getName() +
                    " expected in instance " + id + ", but got " + valueCount);
        }
    }

    private static CardinalityConstraintViolatedException nonEmptyError(Object id, String className, String fieldName) {
        return new CardinalityConstraintViolatedException(
                "Attribute " + className + "." + fieldName + " of instance " + id +
                        " was marked as nonEmpty, but contains no value.");
    }

    private static void validateNonEmpty(Object id, Attribute<?, ?> attribute, int valueCount) {
        if (valueCount == 0 && attribute.isNonEmpty()) {
            throw nonEmptyError(id, attribute.getDeclaringType().getJavaType().getSimpleName(), attribute.getName());
        }
    }
}

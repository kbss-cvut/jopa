/**
 * Copyright (C) 2016 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.sessions.validator;

import cz.cvut.kbss.jopa.exceptions.CardinalityConstraintViolatedException;
import cz.cvut.kbss.jopa.model.annotations.ParticipationConstraint;
import cz.cvut.kbss.jopa.model.annotations.ParticipationConstraints;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.utils.ErrorUtils;

import java.lang.reflect.Field;
import java.util.Collection;
import java.util.Objects;

/**
 * Validator of participation constraints.
 */
class CardinalityConstraintsValidator extends IntegrityConstraintsValidator {

    CardinalityConstraintsValidator() {
    }

    /**
     * Validates cardinality constraints defined in the {@link ParticipationConstraints} annotation.
     *
     * @param identifier Field owner identifier (individual URI)
     * @param field      Field on which the constraints are defined
     * @param value      The value to validate
     */
    public void validate(Object identifier, Field field, Object value) {
        Objects.requireNonNull(field, ErrorUtils.getNPXMessageSupplier("field"));
        final ParticipationConstraints constraints = field.getAnnotation(ParticipationConstraints.class);
        if (constraints == null) {
            return;
        }
        final int valueCount = extractValueCount(value);
        for (ParticipationConstraint pc : constraints.value()) {
            validateParticipationConstraint(identifier, field, valueCount, pc);
        }
        if (constraints.value().length == 0) {
            validateNonEmpty(identifier, field, valueCount, constraints);
        }
    }

    @Override
    public void validate(Object identifier, FieldSpecification<?, ?> attribute, Object attributeValue) {
        if (!(attribute instanceof Attribute)) {
            // Only proper attributes can have cardinality constraints
            return;
        }
        final Attribute<?, ?> att = (Attribute<?, ?>) attribute;
        final int valueCount = extractValueCount(attributeValue);
        for (ParticipationConstraint pc : att.getConstraints()) {
            validateParticipationConstraint(identifier, att.getJavaField(), valueCount, pc);
        }
        if (att.getConstraints().length == 0) {
            validateNonEmpty(identifier, att, valueCount);
        }
    }

    private static int extractValueCount(Object value) {
        if (value == null) {
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

    private  static void validateNonEmpty(Object id, Field field, int valueCount, ParticipationConstraints constraints) {
        if (valueCount == 0 && constraints.nonEmpty()) {
            throw nonEmptyError(id, field.getDeclaringClass().getSimpleName(), field.getName());
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

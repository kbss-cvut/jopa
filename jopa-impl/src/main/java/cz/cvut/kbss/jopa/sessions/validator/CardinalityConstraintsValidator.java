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
     * @param field Field on which the constraints are defined
     * @param value The value to validate
     */
    public void validate(Field field, Object value) {
        Objects.requireNonNull(field, ErrorUtils.constructNPXMessage("field"));
        final ParticipationConstraints constraints = field.getAnnotation(ParticipationConstraints.class);
        if (constraints == null) {
            return;
        }
        final int valueCount = extractValueCount(value);
        for (ParticipationConstraint pc : constraints.value()) {
            validateParticipationConstraint(field, valueCount, pc);
        }
        if (constraints.value().length == 0) {
            validateNonEmpty(field, valueCount, constraints);
        }
    }

    @Override
    public void validate(FieldSpecification<?, ?> attribute, Object attributeValue) {
        if (!(attribute instanceof Attribute)) {
            // Only proper attributes can have cardinality constraints
            return;
        }
        final Attribute<?, ?> att = (Attribute<?, ?>) attribute;
        final int valueCount = extractValueCount(attributeValue);
        for (ParticipationConstraint pc : att.getConstraints()) {
            validateParticipationConstraint(att.getJavaField(), valueCount, pc);
        }
        if (att.getConstraints().length == 0) {
            validateNonEmpty(att, valueCount);
        }
    }

    private int extractValueCount(Object value) {
        if (value == null) {
            return 0;
        }
        return value instanceof Collection ? ((Collection<?>) value).size() : 1;
    }

    private void validateParticipationConstraint(Field field, int valueCount, ParticipationConstraint pc) {
        if (valueCount < pc.min()) {
            throw new CardinalityConstraintViolatedException("At least " + pc.min() +
                    " values of attribute " + field.getDeclaringClass().getSimpleName() + "." + field.getName() +
                    " expected, but got only " + valueCount);
        }
        if (pc.max() >= 0 && pc.max() < valueCount) {
            throw new CardinalityConstraintViolatedException("At most " + pc.max() +
                    " values of attribute " + field.getDeclaringClass().getSimpleName() + "." + field.getName() +
                    " expected, but got " + valueCount);
        }
    }

    private void validateNonEmpty(Field field, int valueCount, ParticipationConstraints constraints) {
        if (valueCount == 0 && constraints.nonEmpty()) {
            throw new CardinalityConstraintViolatedException(
                    "Attribute " + field.getDeclaringClass() + "." + field.getName() +
                            " was marked as nonEmpty, but contains no value.");
        }
    }

    private void validateNonEmpty(Attribute<?, ?> attribute, int valueCount) {
        if (valueCount == 0 && attribute.isNonEmpty()) {
            throw new CardinalityConstraintViolatedException(
                    "Attribute " + attribute.getDeclaringType().getJavaType().getSimpleName() + "." +
                            attribute.getName() + " was marked as nonEmpty, but contains no value.");
        }
    }
}

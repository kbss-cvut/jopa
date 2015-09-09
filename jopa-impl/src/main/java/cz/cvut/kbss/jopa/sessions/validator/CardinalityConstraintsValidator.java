package cz.cvut.kbss.jopa.sessions.validator;

import cz.cvut.kbss.jopa.exceptions.CardinalityConstraintViolatedException;
import cz.cvut.kbss.jopa.model.annotations.ParticipationConstraint;
import cz.cvut.kbss.jopa.model.annotations.ParticipationConstraints;
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
     * Validates cardinality constraints defined in the ParticipationConstraint annotation.
     *
     * @param field Field on which the constraints are defined
     * @param value The value to validate
     */
    public void validate(Field field, Object value) {
        Objects.requireNonNull(field, ErrorUtils.constructNPXMessage("field"));
        final ParticipationConstraints constraints = field.getAnnotation(ParticipationConstraints.class);
        if (constraints == null || constraints.value().length == 0) {
            return;
        }
        final int valueCount = extractValueCount(value);
        for (ParticipationConstraint pc : constraints.value()) {
            if (valueCount < pc.min()) {
                throw new CardinalityConstraintViolatedException("At least " + pc.min() +
                        " values of attribute " + field.getName() + " of type " + field.getDeclaringClass() +
                        " expected, but got only " + valueCount);
            }
            if (pc.max() >= 0 && pc.max() < valueCount) {
                throw new CardinalityConstraintViolatedException("At most " + pc.max() +
                        " values of attribute " + field.getName() + " of type " + field.getDeclaringClass() +
                        " expected, but got " + valueCount);
            }
        }
    }

    private static int extractValueCount(Object value) {
        if (value == null) {
            return 0;
        }
        return value instanceof Collection ? ((Collection<?>) value).size() : 1;
    }
}

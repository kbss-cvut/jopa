package cz.cvut.kbss.jopa.utils;

import cz.cvut.kbss.jopa.exceptions.CardinalityConstraintViolatedException;
import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.annotations.ParticipationConstraint;
import cz.cvut.kbss.jopa.model.annotations.ParticipationConstraints;
import cz.cvut.kbss.jopa.sessions.ChangeRecord;
import cz.cvut.kbss.jopa.sessions.ObjectChangeSet;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.Collection;
import java.util.Objects;

/**
 * Created by kidney on 12/15/14.
 */
public abstract class CardinalityConstraintsValidation {

    private CardinalityConstraintsValidation() {
        throw new AssertionError();
    }

    public static void validateCardinalityConstraints(Object instance) {
        Objects.requireNonNull(instance, ErrorUtils.constructNPXMessage("instance"));
        final Class<?> cls = instance.getClass();
        for (Field f : cls.getDeclaredFields()) {
            if (Modifier.isStatic(f.getModifiers())) {
                continue;
            }
            if (!f.isAccessible()) {
                f.setAccessible(true);
            }
            try {
                validateCardinalityConstraints(f, f.get(instance));
            } catch (IllegalAccessException e) {
                throw new OWLPersistenceException(e);
            }
        }
    }

    /**
     * Validates cardinality constraints for changes in the specified change set.
     *
     * @param changeSet The change set to validate
     */
    public static void validateCardinalityConstraints(ObjectChangeSet changeSet) {
        Objects.requireNonNull(changeSet, ErrorUtils.constructNPXMessage("changeSet"));
        for (ChangeRecord record : changeSet.getChanges()) {
            try {
                final Field field = changeSet.getObjectClass().getDeclaredField(record.getAttributeName());
                validateCardinalityConstraints(field, record.getNewValue());
            } catch (NoSuchFieldException e) {
                throw new OWLPersistenceException("Fatal error: field " + record.getAttributeName() + " not found in entity "
                        + changeSet.getObjectClass());
            }
        }
    }

    /**
     * Validates cardinality constraints defined in the ParticipationConstraint annotation.
     *
     * @param field Field on which the constraints are defined
     * @param value The value to validate
     */
    public static void validateCardinalityConstraints(Field field, Object value) {
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

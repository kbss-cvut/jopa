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
package cz.cvut.kbss.jopa.sessions.validator;

import cz.cvut.kbss.jopa.model.annotations.FetchType;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.api.ChangeRecord;
import cz.cvut.kbss.jopa.api.ObjectChangeSet;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;

import java.util.Objects;
import java.util.function.Predicate;
import java.util.stream.Stream;

public abstract class IntegrityConstraintsValidator {

    private static final IntegrityConstraintsValidator generalValidator = initGeneralValidator();

    private static IntegrityConstraintsValidator initGeneralValidator() {
        final GeneralIntegrityConstraintsValidator validator = new GeneralIntegrityConstraintsValidator();
        validator.addValidator(new CardinalityConstraintsValidator());
        return validator;
    }


    public static IntegrityConstraintsValidator getValidator() {
        return generalValidator;
    }

    /**
     * Validates integrity constraints of all attributes of the specified instance.
     *
     * @param instance The instance to validate
     * @param et       EntityType of the instance
     * @param filters  Filters allowing to specify attributes whose validation should be skipped
     * @param <T>      Entity class type
     */
    @SafeVarargs
    public final <T> void validate(T instance, EntityType<T> et, Predicate<FieldSpecification<? super T, ?>>... filters) {
        Objects.requireNonNull(instance);
        Objects.requireNonNull(et);

        final Object id = EntityPropertiesUtils.getIdentifier(instance, et);
        et.getFieldSpecifications().stream()
                .filter(att -> Stream.of(filters).allMatch(p -> p.test(att)))
                .forEach(att -> {
                    final Object value = EntityPropertiesUtils.getAttributeValue(att, instance);
                    validate(id, att, value);
                });
    }

    /**
     * Validates integrity constraints for changes in the specified change set.
     *
     * @param changeSet The change set to validate
     * @param metamodel Metamodel of the persistence unit
     */
    public void validate(ObjectChangeSet changeSet, Metamodel metamodel) {
        Objects.requireNonNull(changeSet);
        Objects.requireNonNull(metamodel);

        final EntityType<?> et = metamodel.entity(changeSet.getObjectClass());
        final Object id = EntityPropertiesUtils.getIdentifier(changeSet.getCloneObject(), et);
        for (ChangeRecord change : changeSet.getChanges()) {
            validate(id, change.getAttribute(), change.getNewValue());
        }
    }

    /**
     * Validates whether the specified value conforms to the attribute integrity constraints.
     *
     * @param identifier     Instance identifier
     * @param attribute      Attribute metadata with integrity constraints
     * @param attributeValue Value to be validated
     */
    public abstract void validate(Object identifier, FieldSpecification<?, ?> attribute, Object attributeValue);

    /**
     * Creates a predicate filtering attributes whose fetch type is {@link FetchType#LAZY}.
     *
     * @param <T> Entity type
     * @return Predicate
     */
    public static <T> Predicate<FieldSpecification<? super T, ?>> isNotLazy() {
        return att -> att.getFetchType() != FetchType.LAZY;
    }

    /**
     * Creates a predicate filtering attributes that contain inferred values.
     *
     * @param <T> Entity type
     * @return Predicate
     */
    public static <T> Predicate<FieldSpecification<? super T, ?>> isNotInferred() {
        return att -> !att.isInferred();
    }
}

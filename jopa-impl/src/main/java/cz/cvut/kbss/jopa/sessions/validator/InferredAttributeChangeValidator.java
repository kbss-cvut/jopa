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

import cz.cvut.kbss.jopa.exceptions.InferredAttributeModifiedException;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.sessions.ConnectionWrapper;
import cz.cvut.kbss.ontodriver.model.Axiom;

import java.net.URI;
import java.util.Collections;
import java.util.HashSet;
import java.util.Optional;
import java.util.Set;

/**
 * Verifies whether a change to an inferred attribute is valid.
 * <p>
 * Changes to attributes possibly containing inferred values have to be validated before merging into storage. This is
 * because if the value is inferred, it cannot be directly removed from the repository, as it is deduced from other
 * statements. Therefore, this validator checks for value removals (update to a singular attribute is a sequence of
 * remove and add in this context) and verifies that none of the removed statements are inferred in the storage. If they
 * are, a {@link InferredAttributeModifiedException} is thrown.
 */
public class InferredAttributeChangeValidator {

    private final ConnectionWrapper connectionWrapper;

    public InferredAttributeChangeValidator(ConnectionWrapper connectionWrapper) {
        this.connectionWrapper = connectionWrapper;
    }

    /**
     * Checks whether the changes to the specified attribute on the specified instance are valid w.r.t. inference in the
     * repository.
     *
     * @param instance           The changed instance
     * @param original           Object containing original values
     * @param fieldSpec          Specification of the modified field
     * @param instanceDescriptor Instance descriptor for repository context resolution
     * @param <T>                Instance type
     */
    public <T> void validateChange(T instance, T original, FieldSpecification<? super T, ?> fieldSpec,
                                   Descriptor instanceDescriptor) {
        final Optional<Axiom<?>> removedInferredValue = getRemovedInferredValues(instance, original, fieldSpec, instanceDescriptor);
        if (removedInferredValue.isPresent()) {
            throw new InferredAttributeModifiedException("Value " + removedInferredValue.get()
                                                                                        .getValue() + " of attribute " + fieldSpec + " is inferred and cannot be removed!");
        }
    }

    private <T> Optional<Axiom<?>> getRemovedInferredValues(T instance, T original,
                                                            FieldSpecification<? super T, ?> fieldSpec,
                                                            Descriptor instanceDescriptor) {
        final Set<Axiom<?>> originalValues = new HashSet<>(
                connectionWrapper.getAttributeAxioms(original, fieldSpec, instanceDescriptor));
        final Set<Axiom<?>> newValues = connectionWrapper.getAttributeAxioms(instance, fieldSpec, instanceDescriptor);
        originalValues.removeAll(newValues);
        if (originalValues.isEmpty()) {
            // Short circuit when there are no values to remove
            return Optional.empty();
        }
        final Set<URI> ctx = instanceDescriptor.getSingleAttributeContext(fieldSpec).map(Collections::singleton)
                                               .orElse(Collections.emptySet());
        return originalValues.stream().filter(a -> connectionWrapper.isInferred(a, ctx))
                             .findAny();
    }

    /**
     * Checks whether a change to the specified attribute on the specified instance involves removal of an inferred
     * value.
     * <p>
     * Removal of inferred values is generally not possible and should lead to an exception or another predictable
     * reject of such a change.
     *
     * @param instance           The changed instance
     * @param original           Object containing original values
     * @param fieldSpec          Specification of the modified field
     * @param instanceDescriptor Instance descriptor for repository context resolution
     * @param <T>                Instance type
     * @return {@code true} when changed instance removes inferred value from the specified attribute, {@code false}
     * otherwise
     */
    public <T> boolean isInferredValueRemoval(T instance, T original, FieldSpecification<? super T, ?> fieldSpec,
                                              Descriptor instanceDescriptor) {
        return getRemovedInferredValues(instance, original, fieldSpec, instanceDescriptor).isPresent();
    }
}

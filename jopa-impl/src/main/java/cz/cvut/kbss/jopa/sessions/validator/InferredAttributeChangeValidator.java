package cz.cvut.kbss.jopa.sessions.validator;

import cz.cvut.kbss.jopa.exceptions.InferredAttributeModifiedException;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.sessions.ConnectionWrapper;
import cz.cvut.kbss.ontodriver.model.Axiom;

import java.net.URI;
import java.util.Collections;
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
        final Set<Axiom<?>> originalValues =
                connectionWrapper.getAttributeAxioms(original, fieldSpec, instanceDescriptor);
        final Set<Axiom<?>> newValues = connectionWrapper.getAttributeAxioms(instance, fieldSpec, instanceDescriptor);
        originalValues.removeAll(newValues);
        if (originalValues.isEmpty()) {
            // Short circuit when there are no values to remove
            return;
        }
        final Set<URI> ctx = instanceDescriptor.getSingleAttributeContext(fieldSpec).map(Collections::singleton)
                                               .orElse(Collections.emptySet());
        final Optional<Axiom<?>> inferred =
                originalValues.stream().filter(a -> connectionWrapper.isInferred(a, ctx)).findAny();
        if (inferred.isPresent()) {
            throw new InferredAttributeModifiedException("Value " + inferred.get()
                                                                            .getValue() + " of attribute " + fieldSpec + " is inferred and cannot be removed!");
        }
    }
}

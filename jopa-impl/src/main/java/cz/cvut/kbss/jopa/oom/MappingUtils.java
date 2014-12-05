package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.ontodriver_new.model.Axiom;
import cz.cvut.kbss.ontodriver_new.model.Assertion.AssertionType;

public final class MappingUtils {

    private MappingUtils() {
        throw new AssertionError();
    }

    /**
     * Checks whether the specified axioms is a class assertion for an instance
     * of the specified entity type. </p>
     *
     * @param ax Axiom
     * @param et Entity type
     * @return True if the axioms asserts that an individual is of type
     * represented by the entity type
     */
    public static boolean isEntityClassAssertion(Axiom<?> ax, EntityType<?> et) {
        return isClassAssertion(ax) && isEntityClass(ax, et);
    }

    /**
     * Returns true if the specified axiom is a class assertion axiom.
     *
     * @param ax Axiom to check
     * @return true if class assertion
     */
    public static boolean isClassAssertion(Axiom<?> ax) {
        return ax.getAssertion().getType() == AssertionType.CLASS;
    }

    private static boolean isEntityClass(Axiom<?> ax, EntityType<?> et) {
        final String type = et.getIRI().toString();
        final String val = ax.getValue().stringValue();
        return val.equals(type);
    }
}

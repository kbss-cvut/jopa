package cz.cvut.kbss.jopa.oom.datatype;

/**
 * Base class for datatype resolvers.
 * <p>
 * These are used for conversion of {@link cz.cvut.kbss.jopa.model.annotations.OWLDataProperty} values between an object
 * model and the types supported by the OntoDriver API.
 * <p>
 * This default implementation performs no conversion and just returns arguments.
 */
public class ValueResolver {

    /**
     * Transform the object model value to a type supported by the OntoDriver API.
     *
     * @param value Value to transform
     * @return Transformed value
     */
    public Object toAxiom(Object value) {
        return value;
    }

    /**
     * Transforms the OntoDriver API value to a type declared in the object model.
     *
     * @param value Value to transform
     * @return Transformed value
     */
    public Object fromAxiom(Object value) {
        return value;
    }
}

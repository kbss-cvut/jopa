package cz.cvut.kbss.ontodriver.descriptor;

import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;
import cz.cvut.kbss.ontodriver.util.ErrorUtils;

import java.util.*;

public class AxiomValueDescriptor extends AxiomDescriptor {

    private final Map<Assertion, List<Value<?>>> values;

    public AxiomValueDescriptor(NamedResource subject) {
        super(subject);
        this.values = new HashMap<>();
    }

    /**
     * Adds a new value for the specified assertion. </p>
     *
     * @param assertion The assertion
     * @param value     The value to add
     * @return This descriptor
     * @throws NullPointerException if either of the arguments is {@code null}
     */
    public AxiomValueDescriptor addAssertionValue(Assertion assertion, Value<?> value) {
        Objects.requireNonNull(assertion, ErrorUtils.npxMessage("assertion"));
        Objects.requireNonNull(value, ErrorUtils.npxMessage("value"));

        final List<Value<?>> assertionValues = getAssertionList(assertion);
        assertionValues.add(value);
        return this;
    }

    /**
     * Gets values of the specified assertion held by this descriptor. </p>
     *
     * @param assertion The assertion
     * @return Unmodifiable list of values or an empty list, if there are none
     * @throws NullPointerException If the argument is {@code null}
     */
    public List<Value<?>> getAssertionValues(Assertion assertion) {
        Objects.requireNonNull(assertion);
        if (!values.containsKey(assertion)) {
            return Collections.emptyList();
        }
        return Collections.unmodifiableList(values.get(assertion));
    }

    private List<Value<?>> getAssertionList(Assertion assertion) {
        assert assertion != null;
        if (!values.containsKey(assertion)) {
            values.put(assertion, new ArrayList<>());
            addAssertion(assertion);
        }
        return values.get(assertion);
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + values.hashCode();
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (!super.equals(obj))
            return false;
        if (getClass() != obj.getClass())
            return false;
        AxiomValueDescriptor other = (AxiomValueDescriptor) obj;
        if (!values.equals(other.values))
            return false;
        return true;
    }

    @Override
    public String toString() {
        return super.toString() + ", property values: " + values;
    }
}

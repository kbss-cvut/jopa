package cz.cvut.kbss.ontodriver.descriptor;

import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.NamedResource;

import java.net.URI;
import java.util.Objects;
import java.util.Set;

/**
 * Defines common API for axiom descriptors.
 */
public abstract class AbstractAxiomDescriptor {

    private final NamedResource subject;

    protected AbstractAxiomDescriptor(NamedResource subject) {
        this.subject = Objects.requireNonNull(subject);
    }

    public NamedResource getSubject() {
        return subject;
    }

    /**
     * Gets the set of assertions in this descriptor.
     *
     * @return Set of assertions
     */
    public abstract Set<Assertion> getAssertions();

    /**
     * Checks whether this descriptor contains the specified assertion.
     *
     * @param assertion Assertion to look for
     * @return {@code boolean} result
     */
    public abstract boolean containsAssertion(Assertion assertion);

    /**
     * Gets the set of repository context identifiers in which this descriptor's subject may be.
     *
     * @return Set of context identifiers
     */
    public abstract Set<URI> getSubjectContexts();

    /**
     * Gets the set of repository context identifiers in which the specified assertion values may be.
     *
     * @return Set of context identifiers
     */
    public abstract Set<URI> getAssertionContexts(Assertion assertion);

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof AbstractAxiomDescriptor)) return false;
        AbstractAxiomDescriptor that = (AbstractAxiomDescriptor) o;
        return subject.equals(that.subject);
    }

    @Override
    public int hashCode() {
        return Objects.hash(subject);
    }
}

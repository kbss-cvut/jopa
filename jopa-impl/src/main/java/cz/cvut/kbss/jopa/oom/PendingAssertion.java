package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.NamedResource;

import java.net.URI;

/**
 * Pending assertion represents reference to an instance which has not been persisted, yet.
 */
class PendingAssertion {
    private final NamedResource owner;
    private final Assertion assertion;
    private final URI context;


    PendingAssertion(NamedResource owner, Assertion assertion, URI context) {
        this.owner = owner;
        this.assertion = assertion;
        this.context = context;
    }

    public NamedResource getOwner() {
        return owner;
    }

    public Assertion getAssertion() {
        return assertion;
    }

    public URI getContext() {
        return context;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        PendingAssertion that = (PendingAssertion) o;

        return owner.equals(that.owner) && assertion.equals(that.assertion) && (context != null ?
                context.equals(that.context) : that.context == null);
    }

    @Override
    public int hashCode() {
        int result = owner.hashCode();
        result = 31 * result + assertion.hashCode();
        result = 31 * result + (context != null ? context.hashCode() : 0);
        return result;
    }

    @Override
    public String toString() {
        return "PendingAssertion{" +
                owner + " -> " + assertion +
                ", context=" + context +
                '}';
    }
}

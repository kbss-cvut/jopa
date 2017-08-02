package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.net.URI;
import java.util.*;

/**
 * Used to track references to unpersisted instances during transaction.
 * <p>
 * The general rule is that on commit, this registry should be empty.
 */
class PendingAssertionRegistry {

    private static final Logger LOG = LoggerFactory.getLogger(PendingAssertionRegistry.class);

    private final Map<Object, Set<PendingAssertion>> pendingAssertions = new HashMap<>();

    /**
     * Registers a new pending assertion.
     *
     * @param owner     Subject of the assertion
     * @param assertion The assertion representation
     * @param object    The value of the assertion. Always an individual identifier
     * @param context   Context into which the assertion will be added
     */
    void addPendingAssertion(NamedResource owner, Assertion assertion, Object object, URI context) {
        assert owner != null;
        assert assertion != null;
        assert object != null;

        final PendingAssertion pa = new PendingAssertion(owner, assertion, context);
        if (!pendingAssertions.containsKey(object)) {
            pendingAssertions.put(object, new HashSet<>());
        }
        pendingAssertions.get(object).add(pa);
    }

    /**
     * Removes any pending persists with the specified object (value of the assertion).
     *
     * @param object Object which is no longer considered pending, so all assertions referencing it should be removed
     *               from this registry
     */
    Set<PendingAssertion> removeAndGetPendingAssertionsWith(Object object) {
        assert object != null;
        final Set<PendingAssertion> pending = pendingAssertions.remove(object);
        return pending != null ? pending : Collections.emptySet();
    }

    Set<Object> getPendingResources() {
        return Collections.unmodifiableSet(pendingAssertions.keySet());
    }

    /**
     * Removes all pending assertions which have the same subject (owner).
     *
     * @param subject The subject of assertions to remove
     */
    void removePendingAssertions(NamedResource subject) {
        if (LOG.isTraceEnabled()) {
            LOG.trace("Removing pending assertions for subject {}.", subject);
        }
        for (Set<PendingAssertion> pending : pendingAssertions.values()) {
            pending.removeIf(item -> item.owner.equals(subject));
        }
        pendingAssertions.entrySet().removeIf(e -> e.getValue().isEmpty());
    }

    public static class PendingAssertion {
        private final NamedResource owner;
        private final Assertion assertion;
        private final URI context;


        private PendingAssertion(NamedResource owner, Assertion assertion, URI context) {
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
}

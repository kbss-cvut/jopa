package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.ontodriver.descriptor.ListValueDescriptor;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.net.URI;
import java.util.*;

/**
 * Used to track references to unpersisted instances during transaction.
 * <p>
 * The general rule is that on commit, this registry must be empty.
 */
class PendingReferenceRegistry {

    private static final Logger LOG = LoggerFactory.getLogger(PendingReferenceRegistry.class);

    private final Map<Object, Set<PendingAssertion>> pendingAssertions = new HashMap<>();

    private final Map<Object, Set<PendingListReference>> pendingLists = new HashMap<>();
    /**
     * Counts number of pending items per each list
     */
    private final Map<ListValueDescriptor, Integer> pendingListItems = new HashMap<>();

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
     * Registers a pending reference to a sequence (simple or referenced).
     *
     * @param item            The pending (unpersisted) item
     * @param valueDescriptor Descriptor containing info about list owner, linking property etc.
     * @param values          Values of the sequence
     */
    void addPendingListReference(Object item, ListValueDescriptor valueDescriptor, List<?> values) {
        pendingLists.putIfAbsent(item, new HashSet<>());
        pendingLists.get(item).add(new PendingListReference(valueDescriptor, values));
        pendingListItems.compute(valueDescriptor, (k, v) -> v == null ? 1 : v + 1);
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

    Set<PendingListReference> removeAndGetPendingListReferencesWith(Object object) {
        assert object != null;
        final Set<PendingListReference> refs = pendingLists.remove(object);
        if (refs == null) {
            return Collections.emptySet();
        }
        final Iterator<PendingListReference> it = refs.iterator();
        while (it.hasNext()) {
            final PendingListReference ref = it.next();
            pendingListItems.compute(ref.descriptor, (k, v) -> v - 1);
            if (pendingListItems.get(ref.descriptor) == 0) {
                pendingListItems.remove(ref.descriptor);
            } else {
                it.remove();
            }
        }
        return refs;
    }

    Set<Object> getPendingResources() {
        final Set<Object> pending = new HashSet<>(pendingAssertions.keySet());
        pending.addAll(pendingLists.keySet());
        return pending;
    }

    boolean hasPendingResources() {
        return !pendingAssertions.isEmpty() || !pendingLists.isEmpty();
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
            pending.removeIf(item -> item.getOwner().equals(subject));
        }
        pendingAssertions.entrySet().removeIf(e -> e.getValue().isEmpty());
    }

    /**
     * Removes pending assertions representing the specified assertion about the specified subject.
     *
     * @param subject   Assertion subject
     * @param assertion The assertion
     */
    void removePendingAssertions(NamedResource subject, Assertion assertion) {
        if (LOG.isTraceEnabled()) {
            LOG.trace("Removing pending assertions {} for subject {}.", assertion, subject);
        }
        for (Set<PendingAssertion> pending : pendingAssertions.values()) {
            pending.removeIf(item -> item.getOwner().equals(subject) && item.getAssertion().equals(assertion));
        }
        pendingAssertions.entrySet().removeIf(e -> e.getValue().isEmpty());
    }

    static class PendingListReference {
        private final ListValueDescriptor descriptor;
        private final List<?> values;

        private PendingListReference(ListValueDescriptor descriptor, List<?> values) {
            this.descriptor = descriptor;
            this.values = values;
        }

        public ListValueDescriptor getDescriptor() {
            return descriptor;
        }

        public List<?> getValues() {
            return values;
        }
    }
}

/**
 * Copyright (C) 2022 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.ontodriver.descriptor;

import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.NamedResource;

import java.net.URI;
import java.util.*;

/**
 * This descriptor specifies subject and properties of axioms to search for in the ontology.
 * <p>
 * Additionally, it can specify context URIs for both the descriptor and individual properties so that the underlying
 * driver knows where to look for the corresponding axioms.
 */
public class AxiomDescriptor extends AbstractAxiomDescriptor {

    private final Set<URI> subjectContexts = new HashSet<>(4);
    private final Map<Assertion, Set<URI>> assertionContexts;

    public AxiomDescriptor(NamedResource subject) {
        super(subject);
        this.assertionContexts = new HashMap<>();
    }

    /**
     * Adds subject context.
     * <p>
     * The context can be {@code null}, indicating the default context.
     *
     * @param context The context to use for subject
     * @return This instance
     */
    public AxiomDescriptor addSubjectContext(URI context) {
        if (context == null) {
            subjectContexts.clear();
        } else {
            subjectContexts.add(context);
        }
        return this;
    }

    /**
     * Adds the specified assertion to this descriptor.
     *
     * @param assertion The assertion to add
     * @throws NullPointerException When {@code assertion} is {@code null}
     */
    public void addAssertion(Assertion assertion) {
        Objects.requireNonNull(assertion);
        assertionContexts.put(assertion, null);
    }

    /**
     * Sets context for the specified assertion.
     * <p>
     * Note that the assertion has to be already present in this descriptor.
     *
     * @param assertion The property to set context for
     * @param context   Context URI. Use {@code null} to indicate the default context
     * @return This instance
     * @throws IllegalArgumentException If there is no such assertion in this descriptor
     * @throws NullPointerException     When {@code assertion} is {@code null}
     */
    public AxiomDescriptor addAssertionContext(Assertion assertion, URI context) {
        Objects.requireNonNull(assertion);
        if (!assertionContexts.containsKey(assertion)) {
            throw new IllegalArgumentException("Assertion " + assertion + " is not present in this descriptor.");
        }
        final Set<URI> contexts = assertionContexts.computeIfAbsent(assertion, a -> new HashSet<>());
        if (context == null) {
            contexts.clear();
        } else {
            contexts.add(context);
        }
        return this;
    }

    /**
     * Gets unmodifiable view of the properties in this descriptor.
     *
     * @return Set of assertions in this descriptor
     */
    public Set<Assertion> getAssertions() {
        return Collections.unmodifiableSet(assertionContexts.keySet());
    }

    /**
     * Checks whether this descriptor contains the specified assertion.
     *
     * @param assertion The assertion to check
     * @return True if the assertion is already present in this descriptor, false otherwise
     */
    public boolean containsAssertion(Assertion assertion) {
        return assertionContexts.containsKey(assertion);
    }

    @Override
    public Set<URI> getSubjectContexts() {
        return Collections.unmodifiableSet(subjectContexts);
    }

    @Override
    public Set<URI> getAssertionContexts(Assertion assertion) {
        Objects.requireNonNull(assertion);
        if (!assertionContexts.containsKey(assertion) || assertionContexts.get(assertion) == null) {
            return getSubjectContexts();
        }
        return Collections.unmodifiableSet(assertionContexts.get(assertion));
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof AxiomDescriptor)) {
            return false;
        }
        if (!super.equals(o)) {
            return false;
        }
        AxiomDescriptor that = (AxiomDescriptor) o;
        return subjectContexts.equals(that.subjectContexts) && assertionContexts.equals(that.assertionContexts);
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), subjectContexts, assertionContexts);
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder();
        sb.append("[").append(getSubject());
        sb.append(" - ").append(subjectContexts);
        if (!assertionContexts.isEmpty()) {
            sb.append(", properties: ").append(assertionContexts);
        }
        sb.append("]");
        return sb.toString();
    }
}

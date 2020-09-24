/**
 * Copyright (C) 2020 Czech Technical University in Prague
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

    private final Set<Assertion> assertions;

    private URI subjectContext;
    private final Map<Assertion, URI> assertionContexts;

    public AxiomDescriptor(NamedResource subject) {
        super(subject);
        this.assertions = new HashSet<>();
        this.assertionContexts = new HashMap<>();
    }

    /**
     * Sets subject context.
     *
     * @param context The context to use for subject
     */
    public void setSubjectContext(URI context) {
        this.subjectContext = context;
    }

    /**
     * Adds the specified assertion to this descriptor.
     *
     * @param assertion The assertion to add
     * @throws NullPointerException When {@code assertion} is {@code null}
     */
    public void addAssertion(Assertion assertion) {
        Objects.requireNonNull(assertion);
        assertions.add(assertion);
    }

    /**
     * Sets context for the specified assertion.
     * <p>
     * Note that the assertion has to be already present in this descriptor.
     *
     * @param assertion The property to set context for
     * @param context   Context URI
     * @throws IllegalArgumentException If there is no such assertion in this descriptor
     * @throws NullPointerException     When {@code assertion} is {@code null}
     */
    public void setAssertionContext(Assertion assertion, URI context) {
        Objects.requireNonNull(assertion);
        if (!assertions.contains(assertion)) {
            throw new IllegalArgumentException("Assertion " + assertion + " is not present in this descriptor.");
        }
        assertionContexts.put(assertion, context);
    }

    public URI getSubjectContext() {
        return subjectContext;
    }

    /**
     * Gets context of the specified assertion.
     * <p>
     * If the context was not explicitly set, the same context as the subject's is returned.
     *
     * @param assertion Assertion for which context should be resolved
     * @return Assertion context
     */
    public URI getAssertionContext(Assertion assertion) {
        Objects.requireNonNull(assertion);
        if (!assertionContexts.containsKey(assertion)) {
            return subjectContext;
        }
        return assertionContexts.get(assertion);
    }

    /**
     * Gets unmodifiable view of the properties in this descriptor.
     *
     * @return Set of assertions in this descriptor
     */
    public Set<Assertion> getAssertions() {
        return Collections.unmodifiableSet(assertions);
    }

    /**
     * Checks whether this descriptor contains the specified assertion.
     *
     * @param assertion The assertion to check
     * @return True if the assertion is already present in this descriptor, false otherwise
     */
    public boolean containsAssertion(Assertion assertion) {
        return assertions.contains(assertion);
    }

    @Override
    public Set<URI> getSubjectContexts() {
        // TODO
        return Collections.singleton(getSubjectContext());
    }

    @Override
    public Set<URI> getAssertionContexts(Assertion assertion) {
        // TODO
        return Collections.singleton(getAssertionContext(assertion));
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof AxiomDescriptor)) return false;
        if (!super.equals(o)) return false;
        AxiomDescriptor that = (AxiomDescriptor) o;
        return assertions.equals(that.assertions) &&
                Objects.equals(subjectContext, that.subjectContext) &&
                assertionContexts.equals(that.assertionContexts);
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), assertions, subjectContext, assertionContexts);
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder();
        sb.append("[").append(getSubject());
        if (subjectContext != null) {
            sb.append(" - ").append(subjectContext);
        }
        if (!assertionContexts.isEmpty()) {
            sb.append(", properties: ").append(assertionContexts);
        } else {
            sb.append(", properties: ").append(assertions);
        }
        sb.append("]");
        return sb.toString();
    }
}

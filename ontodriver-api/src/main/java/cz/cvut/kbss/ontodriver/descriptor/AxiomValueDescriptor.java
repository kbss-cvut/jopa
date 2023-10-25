/*
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.ontodriver.descriptor;

import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;

import java.net.URI;
import java.util.*;

public class AxiomValueDescriptor extends AbstractAxiomDescriptor {

    private URI subjectContext;

    private final Map<Assertion, AssertionData> assertionData;

    public AxiomValueDescriptor(NamedResource subject) {
        super(subject);
        this.assertionData = new HashMap<>();
    }

    /**
     * Sets subject context.
     *
     * @param context The context to use for subject
     */
    public void setSubjectContext(URI context) {
        this.subjectContext = context;
    }

    public URI getSubjectContext() {
        return subjectContext;
    }

    /**
     * Adds the specified assertion to this descriptor.
     *
     * @param assertion The assertion to add
     * @throws NullPointerException When {@code assertion} is {@code null}
     */
    public void addAssertion(Assertion assertion) {
        Objects.requireNonNull(assertion);
        assertionData.put(assertion, new AssertionData());
    }

    /**
     * Gets unmodifiable view of the properties in this descriptor.
     *
     * @return Set of assertions in this descriptor
     */
    public Set<Assertion> getAssertions() {
        return Collections.unmodifiableSet(assertionData.keySet());
    }

    /**
     * Checks whether this descriptor contains the specified assertion.
     *
     * @param assertion The assertion to check
     * @return True if the assertion is already present in this descriptor, false otherwise
     */
    public boolean containsAssertion(Assertion assertion) {
        return assertionData.containsKey(assertion);
    }

    @Override
    public Set<URI> getSubjectContexts() {
        return subjectContext != null ? Collections.singleton(getSubjectContext()) : Collections.emptySet();
    }

    @Override
    public Set<URI> getAssertionContexts(Assertion assertion) {
        final URI ctx = getAssertionContext(assertion);
        return ctx != null ? Collections.singleton(ctx) : Collections.emptySet();
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
        if (!assertionData.containsKey(assertion) || !assertionData.get(assertion).hasContext) {
            return subjectContext;
        }
        return assertionData.get(assertion).context;
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
        if (!assertionData.containsKey(assertion)) {
            throw new IllegalArgumentException("Assertion " + assertion + " is not present in this descriptor.");
        }
        assertionData.get(assertion).setContext(context);
    }

    /**
     * Adds a new value for the specified assertion.
     *
     * @param assertion The assertion
     * @param value     The value to add
     * @return This descriptor
     * @throws NullPointerException if either of the arguments is {@code null}
     */
    public AxiomValueDescriptor addAssertionValue(Assertion assertion, Value<?> value) {
        Objects.requireNonNull(assertion);
        Objects.requireNonNull(value);

        final List<Value<?>> assertionValues = getAssertionList(assertion);
        assertionValues.add(value);
        return this;
    }

    /**
     * Gets values of the specified assertion held by this descriptor.
     *
     * @param assertion The assertion
     * @return Unmodifiable list of values or an empty list, if there are none
     * @throws NullPointerException If the argument is {@code null}
     */
    public List<Value<?>> getAssertionValues(Assertion assertion) {
        Objects.requireNonNull(assertion);
        if (!assertionData.containsKey(assertion)) {
            return Collections.emptyList();
        }
        return Collections.unmodifiableList(assertionData.get(assertion).values);
    }

    private List<Value<?>> getAssertionList(Assertion assertion) {
        assert assertion != null;
        if (!assertionData.containsKey(assertion)) {
            addAssertion(assertion);
        }
        return assertionData.get(assertion).values;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof AxiomValueDescriptor)) {
            return false;
        }
        if (!super.equals(o)) {
            return false;
        }
        AxiomValueDescriptor that = (AxiomValueDescriptor) o;
        return Objects.equals(subjectContext, that.subjectContext) &&
                assertionData.equals(that.assertionData);
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), subjectContext, assertionData);
    }

    @Override
    public String toString() {
        return "AxiomValueDescriptor{<" + getSubject() + "> - " + assertionData + '}';
    }

    private static class AssertionData {
        private final List<Value<?>> values = new ArrayList<>();
        private URI context;
        private boolean hasContext;

        private void setContext(URI context) {
            this.context = context;
            this.hasContext = true;
        }

        @Override
        public String toString() {
            return "AssertionData{" +
                    "values=" + values +
                    '}';
        }
    }
}

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
     * <p>
     * The contract of this method is as follows: it must not return {@code null}, if the the subject is in the default
     * context, an empty set is returned.
     *
     * @return Set of context identifiers
     */
    public abstract Set<URI> getSubjectContexts();

    /**
     * Gets the set of repository context identifiers in which the specified assertion values may be.
     * <p>
     * If no context was explicitly set, the same contexts as the subject's are returned. An empty result indicates that
     * the default context should be used.
     *
     * @return Set of context identifiers
     */
    public abstract Set<URI> getAssertionContexts(Assertion assertion);

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof AbstractAxiomDescriptor)) {
            return false;
        }
        AbstractAxiomDescriptor that = (AbstractAxiomDescriptor) o;
        return subject.equals(that.subject);
    }

    @Override
    public int hashCode() {
        return Objects.hash(subject);
    }
}

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
package cz.cvut.kbss.ontodriver.model;

import java.util.Objects;

public final class AxiomImpl<T> implements Axiom<T> {

    private final NamedResource subject;
    private final Assertion assertion;
    private final Value<T> value;

    public AxiomImpl(NamedResource subject, Assertion assertion, Value<T> value) {
        this.subject = Objects.requireNonNull(subject);
        this.assertion = Objects.requireNonNull(assertion);
        this.value = Objects.requireNonNull(value);
    }

    @Override
    public NamedResource getSubject() {
        return subject;
    }

    @Override
    public Assertion getAssertion() {
        return assertion;
    }

    @Override
    public Value<T> getValue() {
        return value;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((assertion == null) ? 0 : assertion.hashCode());
        result = prime * result + ((subject == null) ? 0 : subject.hashCode());
        result = prime * result + ((value == null) ? 0 : value.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        AxiomImpl<?> other = (AxiomImpl<?>) obj;
        if (assertion == null) {
            if (other.assertion != null)
                return false;
        } else if (!assertion.equals(other.assertion))
            return false;
        if (subject == null) {
            if (other.subject != null)
                return false;
        } else if (!subject.equals(other.subject))
            return false;
        if (value == null) {
            return other.value == null;
        } else return value.equals(other.value);
    }

    @Override
    public String toString() {
        return "[" + subject + " " + assertion + " " + value + "]";
    }
}

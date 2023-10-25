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
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        AxiomImpl<?> axiom = (AxiomImpl<?>) o;
        return subject.equals(axiom.subject) && assertion.equals(axiom.assertion) && value.equals(axiom.value);
    }

    @Override
    public int hashCode() {
        return Objects.hash(subject, assertion, value);
    }

    @Override
    public String toString() {
        return "[" + subject + " " + assertion + " " + value + "]";
    }
}

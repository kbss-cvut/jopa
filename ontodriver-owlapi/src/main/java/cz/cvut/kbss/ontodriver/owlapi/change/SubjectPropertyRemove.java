/*
 * JOPA
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */
package cz.cvut.kbss.ontodriver.owlapi.change;

import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLProperty;
import org.semanticweb.owlapi.model.OWLPropertyAssertionAxiom;

import java.util.Objects;

public abstract class SubjectPropertyRemove<T extends OWLProperty> implements TransactionalChange {

    protected final OWLNamedIndividual subject;
    protected final T property;

    public SubjectPropertyRemove(OWLNamedIndividual subject, T property) {
        this.subject = subject;
        this.property = property;
    }

    @Override
    public boolean overrides(TransactionalChange existing) {
        if (existing instanceof MutableAddAxiom) {
            final MutableAddAxiom ax = (MutableAddAxiom) existing;
            if (ax.getAxiom() instanceof OWLPropertyAssertionAxiom) {
                final OWLPropertyAssertionAxiom<?, ?> assertionAxiom = (OWLPropertyAssertionAxiom<?, ?>) ax.getAxiom();
                return subject.equals(assertionAxiom.getSubject()) && property.equals(assertionAxiom.getProperty());
            }
        }
        return false;
    }

    public OWLNamedIndividual getSubject() {
        return subject;
    }

    public T getProperty() {
        return property;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof SubjectPropertyRemove)) {
            return false;
        }
        SubjectPropertyRemove<?> that = (SubjectPropertyRemove<?>) o;
        return subject.equals(that.subject) && property.equals(that.property);
    }

    @Override
    public int hashCode() {
        return Objects.hash(subject, property);
    }

    @Override
    public String toString() {
        return "SubjectPropertyRemove{" +
                "subject=" + subject +
                ", property=" + property +
                '}';
    }
}

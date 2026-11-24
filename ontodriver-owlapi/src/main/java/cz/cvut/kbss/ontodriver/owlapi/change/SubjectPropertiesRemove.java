/*
 * JOPA
 * Copyright (C) 2026 Czech Technical University in Prague
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

import org.semanticweb.owlapi.model.OWLAnnotationAssertionAxiom;
import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyChange;
import org.semanticweb.owlapi.model.OWLPropertyAssertionAxiom;
import org.semanticweb.owlapi.model.RemoveAxiom;

import java.util.List;
import java.util.stream.Collectors;

/**
 * Removes all property assertion axioms (data, object, annotation and class) of the subject.
 * <p>
 * Used for an unspecified property assertion in the axiom descriptor - the equivalent of removing all statements
 * sharing the subject in the triple-store drivers.
 */
public record SubjectPropertiesRemove(OWLNamedIndividual subject) implements TransactionalChange {

    private boolean onSubject(OWLAxiom axiom) {
        if (axiom instanceof OWLPropertyAssertionAxiom<?, ?> p) {
            return subject.equals(p.getSubject());
        }
        if (axiom instanceof OWLAnnotationAssertionAxiom a) {
            return subject.getIRI().equals(a.getSubject());
        }
        return false;
    }

    @Override
    public List<OWLOntologyChange> toOwlChanges(OWLOntology targetOntology) {
        return targetOntology.axioms().filter(this::onSubject)
                             .map(ax -> new RemoveAxiom(targetOntology, ax))
                             .collect(Collectors.toList());
    }

    @Override
    public boolean overrides(TransactionalChange existing) {
        return existing instanceof MutableAddAxiom ax && onSubject(ax.getAxiom());
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof SubjectPropertiesRemove(OWLNamedIndividual subject1))) {
            return false;
        }
        return subject.equals(subject1);
    }

    @Override
    public String toString() {
        return "SubjectPropertiesRemove{" + "subject=" + subject + '}';
    }
}

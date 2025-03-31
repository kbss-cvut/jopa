/*
 * JOPA
 * Copyright (C) 2025 Czech Technical University in Prague
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

import org.semanticweb.owlapi.model.AxiomType;
import org.semanticweb.owlapi.model.OWLClassAssertionAxiom;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyChange;
import org.semanticweb.owlapi.model.RemoveAxiom;
import org.semanticweb.owlapi.search.EntitySearcher;

import java.util.List;
import java.util.stream.Collectors;

public class SubjectClassAssertionRemove implements TransactionalChange {

    private final OWLNamedIndividual subject;

    public SubjectClassAssertionRemove(OWLNamedIndividual subject) {
        this.subject = subject;
    }

    @Override
    public List<OWLOntologyChange> toOwlChanges(OWLOntology targetOntology) {
        return EntitySearcher.getTypes(subject, targetOntology).map(cls -> new RemoveAxiom(targetOntology,
                                     targetOntology.getOWLOntologyManager().getOWLDataFactory().getOWLClassAssertionAxiom(cls, subject)))
                             .collect(Collectors.toList());
    }

    @Override
    public boolean overrides(TransactionalChange existing) {
        if (existing instanceof MutableAddAxiom ax) {
            return ax.getAxiom()
                     .isOfType(AxiomType.CLASS_ASSERTION) && subject.equals(((OWLClassAssertionAxiom) ax.getAxiom()).getIndividual());
        }
        return false;
    }
}

/*
 * JOPA
 * Copyright (C) 2024 Czech Technical University in Prague
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

import org.semanticweb.owlapi.model.OWLIndividual;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLObjectProperty;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyChange;
import org.semanticweb.owlapi.model.RemoveAxiom;
import org.semanticweb.owlapi.search.EntitySearcher;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class SubjectObjectPropertyRemove extends SubjectPropertyRemove<OWLObjectProperty> {

    public SubjectObjectPropertyRemove(OWLNamedIndividual subject, OWLObjectProperty property) {
        super(subject, property);
    }

    @Override
    public List<OWLOntologyChange> toOwlChanges(OWLOntology targetOntology) {
        final Stream<OWLIndividual> values = EntitySearcher.getObjectPropertyValues(subject, property, targetOntology);
        return values.filter(OWLIndividual::isNamed).map(value -> new RemoveAxiom(targetOntology,
                             targetOntology.getOWLOntologyManager().getOWLDataFactory()
                                           .getOWLObjectPropertyAssertionAxiom(property, subject, value)))
                     .collect(Collectors.toList());
    }
}

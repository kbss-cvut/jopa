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
package cz.cvut.kbss.jopa.owl2java;

import cz.cvut.kbss.jopa.model.SequencesVocabulary;
import org.semanticweb.owlapi.model.*;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;
import java.util.TreeSet;

class ContextDefinition {

    /**
     * OWLEntities to skip
     */
    private static final Set<IRI> SKIPPED = new HashSet<>(
            Arrays.asList(IRI.create(SequencesVocabulary.c_Collection), IRI.create(SequencesVocabulary.c_List),
                    IRI.create(SequencesVocabulary.c_OWLSimpleList),
                    IRI.create(SequencesVocabulary.c_OWLReferencedList)));

    final Set<OWLAxiom> axioms = new HashSet<>();
    final Set<OWLClass> classes = new TreeSet<>();
    final Set<OWLObjectProperty> objectProperties = new TreeSet<>();
    final Set<OWLDataProperty> dataProperties = new TreeSet<>();
    final Set<OWLAnnotationProperty> annotationProperties = new TreeSet<>();
    final Set<OWLNamedIndividual> individuals = new TreeSet<>();

    IntegrityConstraintSet set;

    void parse() {
        final IntegrityConstraintParser parser = new IntegrityConstraintParser();
        for (final OWLAxiom a : axioms) {
            a.accept(parser);
        }
        this.set = parser.getClassIntegrityConstraintSet();
    }

    void addAxiom(OWLAxiom axiom) {
        axiom.signature().filter(e -> !SKIPPED.contains(e.getIRI())).forEach(this::add);
        axioms.add(axiom);
    }

    void add(OWLEntity e) {
        if (e.isOWLClass()) {
            classes.add(e.asOWLClass());
        } else if (e.isOWLObjectProperty()) {
            objectProperties.add(e.asOWLObjectProperty());
        } else if (e.isOWLDataProperty()) {
            dataProperties.add(e.asOWLDataProperty());
        } else if (e.isOWLAnnotationProperty()) {
            annotationProperties.add(e.asOWLAnnotationProperty());
        } else if (e.isOWLNamedIndividual()) {
            individuals.add(e.asOWLNamedIndividual());
        }
    }
}
/**
 * Copyright (C) 2016 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.owl2java;

import java.util.HashSet;
import java.util.Set;
import org.semanticweb.owlapi.model.OWLAnnotationProperty;
import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLDataProperty;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLObjectProperty;

class ContextDefinition {

    final Set<OWLAxiom> axioms = new HashSet<>();
    final Set<OWLClass> classes = new HashSet<>();
    final Set<OWLObjectProperty> objectProperties = new HashSet<>();
    final Set<OWLDataProperty> dataProperties = new HashSet<>();
    final Set<OWLAnnotationProperty> annotationProperties = new HashSet<>();
    final Set<OWLNamedIndividual> individuals = new HashSet<>();
    final IntegrityConstraintParser parser = new IntegrityConstraintParser();

    private final String name;

    ContextDefinition(String name) {
        this.name = name;
    }
}
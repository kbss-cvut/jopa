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

import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLClass;

import java.util.HashSet;
import java.util.Set;

class ContextDefinition {

    private final String name;
    final Set<OWLClass> classes = new HashSet<>();
    final Set<org.semanticweb.owlapi.model.OWLObjectProperty> objectProperties = new HashSet<>();
    final Set<org.semanticweb.owlapi.model.OWLDataProperty> dataProperties = new HashSet<>();
    final Set<org.semanticweb.owlapi.model.OWLAnnotationProperty> annotationProperties = new HashSet<>();
    final Set<org.semanticweb.owlapi.model.OWLNamedIndividual> individuals = new HashSet<>();

    final Set<OWLAxiom> axioms = new HashSet<>();

    final IntegrityConstraintParserImpl parser = new IntegrityConstraintParserImpl(OWLManager.getOWLDataFactory(),
            this);

    ContextDefinition(String name) {
        this.name = name;
    }
}
/**
 * Copyright (C) 2019 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.owlapi;

import cz.cvut.kbss.ontodriver.owlapi.connector.OntologySnapshot;
import cz.cvut.kbss.ontodriver.owlapi.util.MutableAddAxiom;
import cz.cvut.kbss.ontodriver.descriptor.AxiomValueDescriptor;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.AxiomImpl;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;
import cz.cvut.kbss.ontodriver.owlapi.util.OwlapiUtils;
import org.semanticweb.owlapi.model.*;

import java.net.URI;
import java.util.*;
import java.util.stream.Collectors;

/**
 * Persists axioms into the ontology snapshot.
 */
class AxiomSaver {

    private final OwlapiAdapter adapter;

    private final OWLOntology ontology;
    private final OntologySnapshot snapshot;

    private final AxiomAdapter axiomAdapter;

    AxiomSaver(OwlapiAdapter adapter, OntologySnapshot snapshot) {
        this.adapter = adapter;
        this.snapshot = snapshot;
        this.ontology = snapshot.getOntology();
        this.axiomAdapter = new AxiomAdapter(snapshot.getDataFactory(), adapter.getLanguage());
    }

    void persist(AxiomValueDescriptor descriptor) {
        for (Assertion assertion : descriptor.getAssertions()) {
            switch (assertion.getType()) {
                case CLASS:
                    persistTypes(descriptor.getSubject(), descriptor.getAssertionValues(assertion));
                    break;
                case DATA_PROPERTY:
                    persistDataPropertyValues(descriptor.getSubject(), assertion,
                            descriptor.getAssertionValues(assertion));
                    break;
                case ANNOTATION_PROPERTY:
                    persistAnnotationPropertyValues(descriptor.getSubject(), assertion,
                            descriptor.getAssertionValues(assertion));
                    break;
                case OBJECT_PROPERTY:
                    persistObjectPropertyValues(descriptor.getSubject(), assertion,
                            descriptor.getAssertionValues(assertion));
                    break;
                case PROPERTY:
                    persistPropertyValues(descriptor.getSubject(), assertion, descriptor.getAssertionValues(assertion));
                    break;
            }
        }
    }

    private void persistTypes(NamedResource subject, List<Value<?>> types) {
        final Set<URI> classes = types.stream().map(val -> {
            if (val.getValue() instanceof URI) {
                return (URI) val.getValue();
            } else {
                return URI.create(val.stringValue());
            }
        }).collect(Collectors.toSet());
        adapter.getTypesHandler().addTypes(subject, null, classes);
    }

    private void persistDataPropertyValues(NamedResource subject, Assertion assertion, Collection<Value<?>> values) {
        final List<OWLAxiom> axioms = values.stream().filter(value -> value != Value.nullValue())
                                            .map(value -> axiomAdapter.toOwlDataPropertyAssertionAxiom(
                                                    new AxiomImpl<>(subject, assertion, value)))
                                            .collect(Collectors.toList());
        addAxioms(axioms);
    }

    private void addAxioms(List<? extends OWLAxiom> axioms) {
        if (axioms.isEmpty()) {
            return;
        }
        final List<OWLOntologyChange> changes = axioms.stream().map(axiom -> new MutableAddAxiom(ontology, axiom))
                                                      .collect(Collectors.toList());
        adapter.addTransactionalChanges(snapshot.applyChanges(changes));
    }

    private void persistAnnotationPropertyValues(NamedResource subject, Assertion assertion,
                                                 Collection<Value<?>> values) {
        final List<OWLAxiom> axioms = values.stream().filter(value -> value != Value.nullValue())
                                            .map(value -> axiomAdapter.toOwlAnnotationPropertyAssertionAxiom(
                                                    new AxiomImpl<>(subject, assertion, value)))
                                            .collect(Collectors.toList());
        addAxioms(axioms);
    }

    private void persistObjectPropertyValues(NamedResource subject, Assertion assertion, Collection<Value<?>> values) {
        final List<OWLAxiom> axioms = values.stream().filter(value -> value != Value.nullValue())
                                            .map(value -> {
                                                // Simplistic version using value.stringValue
                                                // We expect the value to  be a NamedResource, but in case the property was unspecified and it was only assumed
                                                // it is an object property (see #persistPropertyValues), the value would be a simple string
                                                return axiomAdapter.toOwlObjectPropertyAssertionAxiom(
                                                        new AxiomImpl<>(subject, assertion, value));
                                            }).collect(Collectors.toList());
        addAxioms(axioms);
    }

    private void persistPropertyValues(NamedResource subject, Assertion assertion, Collection<Value<?>> values) {
        final IRI property = IRI.create(assertion.getIdentifier());
        if (ontology.containsDataPropertyInSignature(property)) {
            persistDataPropertyValues(subject, assertion, values);
        } else if (ontology.containsObjectPropertyInSignature(property)) {
            persistObjectPropertyValues(subject, assertion, values);
        } else if (ontology.containsAnnotationPropertyInSignature(property)) {
            persistAnnotationPropertyValues(subject, assertion, values);
        } else {
            persistUnknownPropertyValues(subject, assertion, values);
        }
    }

    private void persistUnknownPropertyValues(NamedResource subject, Assertion assertion, Collection<Value<?>> values) {
        final List<OWLAxiom> axioms = new ArrayList<>();
        for (Value<?> v : values) {
            if (OwlapiUtils.isIndividualIri(v.getValue())) {
                axioms.add(axiomAdapter.toOwlObjectPropertyAssertionAxiom(new AxiomImpl<>(subject, assertion, v)));
            } else {
                axioms.add(axiomAdapter.toOwlDataPropertyAssertionAxiom(new AxiomImpl<>(subject, assertion, v)));
            }
        }
        addAxioms(axioms);
    }

    void persistAxioms(NamedResource subject, Map<Assertion, Set<Value<?>>> properties) {
        for (Map.Entry<Assertion, Set<Value<?>>> e : properties.entrySet()) {
            persistPropertyValues(subject, e.getKey(), e.getValue());
        }
    }
}

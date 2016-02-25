/**
 * Copyright (C) 2016 Czech Technical University in Prague
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

import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;
import cz.cvut.kbss.ontodriver.owlapi.connector.OntologySnapshot;
import cz.cvut.kbss.ontodriver.owlapi.util.MutableRemoveAxiom;
import cz.cvut.kbss.ontodriver.owlapi.util.OwlapiUtils;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.search.EntitySearcher;

import java.util.*;
import java.util.stream.Collectors;

class EpistemicAxiomRemover {

    private final OwlapiAdapter owlapiAdapter;
    private final OWLOntology ontology;
    private final OWLDataFactory dataFactory;
    private final OntologySnapshot snapshot;

    EpistemicAxiomRemover(OwlapiAdapter adapter, OntologySnapshot snapshot) {
        this.owlapiAdapter = adapter;
        this.snapshot = snapshot;
        this.ontology = snapshot.getOntology();
        this.dataFactory = snapshot.getDataFactory();
    }

    void remove(AxiomDescriptor descriptor) {
        final List<OWLOntologyChange> changes = new ArrayList<>();
        final OWLNamedIndividual individual = OwlapiUtils.getIndividual(descriptor.getSubject(), dataFactory);
        for (Assertion a : descriptor.getAssertions()) {
            switch (a.getType()) {
                case CLASS:
                    changes.addAll(removeClassAssertionAxioms(individual));
                    break;
                case DATA_PROPERTY:
                    changes.addAll(removeDataPropertyAssertions(individual, a));
                    break;
                case OBJECT_PROPERTY:
                    changes.addAll(removeObjectPropertyAssertions(individual, a));
                    break;
                case ANNOTATION_PROPERTY:
                    changes.addAll(removeAnnotationAssertions(individual, a));
            }
        }
        if (!changes.isEmpty()) {
            owlapiAdapter.addTransactionalChanges(snapshot.applyChanges(changes));
        }
    }

    private Collection<OWLOntologyChange> removeClassAssertionAxioms(OWLNamedIndividual individual) {
        final Collection<OWLClassExpression> types = EntitySearcher.getTypes(individual, ontology);
        return types.stream().map(cls -> new MutableRemoveAxiom(ontology,
                dataFactory.getOWLClassAssertionAxiom(cls, individual))).collect(Collectors.toList());
    }

    private Collection<OWLOntologyChange> removeClassAssertionAxioms(OWLNamedIndividual individual,
                                                                     Set<Value<?>> values) {
        return values.stream().map(value -> {
            final OWLClass owlClass = dataFactory.getOWLClass(IRI.create(value.stringValue()));
            return new MutableRemoveAxiom(ontology, dataFactory.getOWLClassAssertionAxiom(owlClass, individual));
        }).collect(Collectors.toList());
    }

    private Collection<? extends OWLOntologyChange> removeDataPropertyAssertions(OWLNamedIndividual individual,
                                                                                 Assertion assertion) {
        final OWLDataProperty dataProperty = dataFactory.getOWLDataProperty(IRI.create(assertion.getIdentifier()));
        final Collection<OWLLiteral> values = EntitySearcher.getDataPropertyValues(individual, dataProperty, ontology);
        return values.stream().map(value -> new MutableRemoveAxiom(ontology,
                dataFactory.getOWLDataPropertyAssertionAxiom(dataProperty, individual, value)))
                     .collect(Collectors.toList());
    }

    private Collection<? extends OWLOntologyChange> removeDataPropertyAssertions(OWLNamedIndividual individual,
                                                                                 Assertion assertion,
                                                                                 Set<Value<?>> values) {
        final OWLDataProperty dataProperty = dataFactory.getOWLDataProperty(IRI.create(assertion.getIdentifier()));
        return values.stream().map(value -> {
            final OWLLiteral literal = OwlapiUtils
                    .createOWLLiteralFromValue(value.getValue(), dataFactory, owlapiAdapter.getLanguage());
            return new MutableRemoveAxiom(ontology,
                    dataFactory.getOWLDataPropertyAssertionAxiom(dataProperty, individual, literal));
        }).collect(Collectors.toList());
    }

    private Collection<? extends OWLOntologyChange> removeObjectPropertyAssertions(OWLNamedIndividual individual,
                                                                                   Assertion assertion) {
        final OWLObjectProperty objProperty = dataFactory.getOWLObjectProperty(IRI.create(assertion.getIdentifier()));
        final Collection<OWLIndividual> values = EntitySearcher
                .getObjectPropertyValues(individual, objProperty, ontology);
        return values.stream().filter(OWLIndividual::isNamed).map(value -> new MutableRemoveAxiom(ontology,
                dataFactory.getOWLObjectPropertyAssertionAxiom(objProperty, individual, value)))
                     .collect(Collectors.toList());
    }

    private Collection<? extends OWLOntologyChange> removeObjectPropertyAssertions(OWLNamedIndividual individual,
                                                                                   Assertion assertion,
                                                                                   Set<Value<?>> values) {
        final OWLObjectProperty objProperty = dataFactory.getOWLObjectProperty(IRI.create(assertion.getIdentifier()));
        return values.stream().map(value -> {
            final OWLIndividual object = OwlapiUtils
                    .getIndividual(NamedResource.create(value.stringValue()), dataFactory);
            return new MutableRemoveAxiom(ontology,
                    dataFactory.getOWLObjectPropertyAssertionAxiom(objProperty, individual, object));
        }).collect(Collectors.toList());
    }

    private Collection<? extends OWLOntologyChange> removeAnnotationAssertions(OWLNamedIndividual individual,
                                                                               Assertion assertion) {
        final OWLAnnotationProperty annProperty = dataFactory
                .getOWLAnnotationProperty(IRI.create(assertion.getIdentifier()));
        final Collection<OWLAnnotationAssertionAxiom> values = EntitySearcher
                .getAnnotationAssertionAxioms(individual.getIRI(), ontology);
        return values.stream().filter(axiom -> axiom.getProperty().equals(annProperty))
                     .map(value -> new MutableRemoveAxiom(ontology, value)).collect(Collectors.toList());
    }

    private Collection<? extends OWLOntologyChange> removeAnnotationAssertions(OWLNamedIndividual individual,
                                                                               Assertion assertion,
                                                                               Set<Value<?>> values) {
        final OWLAnnotationProperty annProperty = dataFactory
                .getOWLAnnotationProperty(IRI.create(assertion.getIdentifier()));
        return values.stream().map(value -> {
            OWLAnnotationValue av = null;
            try {
                av = IRI.create(value.stringValue());
            } catch (IllegalArgumentException e) {
                av = OwlapiUtils.createOWLLiteralFromValue(value.getValue(), dataFactory, owlapiAdapter.getLanguage());
            }
            assert av != null;
            return new MutableRemoveAxiom(ontology,
                    dataFactory.getOWLAnnotationAssertionAxiom(annProperty, individual.getIRI(), av));
        }).collect(Collectors.toList());
    }

    void removeAxioms(NamedResource subject, Map<Assertion, Set<Value<?>>> toRemove) {
        final List<OWLOntologyChange> changes = new ArrayList<>();
        final OWLNamedIndividual individual = OwlapiUtils.getIndividual(subject, dataFactory);
        for (Map.Entry<Assertion, Set<Value<?>>> e : toRemove.entrySet()) {
            final IRI assertionIri = IRI.create(e.getKey().getIdentifier());
            if (ontology.containsDataPropertyInSignature(assertionIri)) {
                changes.addAll(removeDataPropertyAssertions(individual, e.getKey(), e.getValue()));
            } else if (ontology.containsObjectPropertyInSignature(assertionIri)) {
                changes.addAll(removeObjectPropertyAssertions(individual, e.getKey(), e.getValue()));
            } else if (ontology.containsAnnotationPropertyInSignature(assertionIri)) {
                changes.addAll(removeAnnotationAssertions(individual, e.getKey(), e.getValue()));
            } else if (e.getKey().isClassAssertion()) {
                changes.addAll(removeClassAssertionAxioms(individual, e.getValue()));
            } else {
                throw new IllegalArgumentException("Unknown assertion " + e.getKey());
            }
        }
        owlapiAdapter.addTransactionalChanges(snapshot.applyChanges(changes));
    }
}

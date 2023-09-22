/**
 * Copyright (C) 2023 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License along with this program. If not, see
 * <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.ontodriver.owlapi;

import cz.cvut.kbss.ontodriver.descriptor.AbstractAxiomDescriptor;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;
import cz.cvut.kbss.ontodriver.owlapi.change.MutableRemoveAxiom;
import cz.cvut.kbss.ontodriver.owlapi.change.TransactionalChange;
import cz.cvut.kbss.ontodriver.owlapi.change.SubjectAnnotationPropertyRemove;
import cz.cvut.kbss.ontodriver.owlapi.change.SubjectClassAssertionRemove;
import cz.cvut.kbss.ontodriver.owlapi.change.SubjectDataPropertyRemove;
import cz.cvut.kbss.ontodriver.owlapi.change.SubjectObjectPropertyRemove;
import cz.cvut.kbss.ontodriver.owlapi.connector.OntologySnapshot;
import cz.cvut.kbss.ontodriver.owlapi.util.OwlapiUtils;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLAnnotationProperty;
import org.semanticweb.owlapi.model.OWLAnnotationValue;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLDataProperty;
import org.semanticweb.owlapi.model.OWLIndividual;
import org.semanticweb.owlapi.model.OWLLiteral;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLObjectProperty;
import org.semanticweb.owlapi.model.OWLOntology;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;
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

    void remove(AbstractAxiomDescriptor descriptor) {
        final List<TransactionalChange> changes = new ArrayList<>();
        final OWLNamedIndividual individual = OwlapiUtils.getIndividual(descriptor.getSubject(), dataFactory);
        for (Assertion a : descriptor.getAssertions()) {
            switch (a.getType()) {
                case CLASS:
                    changes.add(new SubjectClassAssertionRemove(individual));
                    break;
                case DATA_PROPERTY:
                    changes.add(new SubjectDataPropertyRemove(individual, dataFactory.getOWLDataProperty(IRI.create(a.getIdentifier()))));
                    break;
                case OBJECT_PROPERTY:
                    changes.add(new SubjectObjectPropertyRemove(individual, dataFactory.getOWLObjectProperty(IRI.create(a.getIdentifier()))));
                    break;
                case ANNOTATION_PROPERTY:
                    changes.add(new SubjectAnnotationPropertyRemove(individual, dataFactory.getOWLAnnotationProperty(IRI.create(a.getIdentifier()))));
                    break;
                default:
                    break;
            }
        }
        if (!changes.isEmpty()) {
            owlapiAdapter.addTransactionalChanges(snapshot.applyChanges(changes));
        }
    }

    void removeAxioms(NamedResource subject, Map<Assertion, Set<Value<?>>> toRemove) {
        final List<TransactionalChange> changes = new ArrayList<>();
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
            }
            // It can happen that the assertionIri is no longer in the ontology, because of the way properties changes
            // are processed in JOPA - they are compared to the original object, so if multiple property values are removed
            // in a transaction, they are effectively removed multiple times. Therefore, it can happen that another
            // property no longer exists in the ontology, because it was removed in the previous modifications during the same
            // transaction
        }
        owlapiAdapter.addTransactionalChanges(snapshot.applyChanges(changes));
    }

    private Collection<TransactionalChange> removeClassAssertionAxioms(OWLNamedIndividual individual, Set<Value<?>> values) {
        return values.stream().map(value -> {
            final OWLClass owlClass = dataFactory.getOWLClass(IRI.create(value.stringValue()));
            return new MutableRemoveAxiom(ontology, dataFactory.getOWLClassAssertionAxiom(owlClass, individual));
        }).collect(Collectors.toList());
    }

    private Collection<TransactionalChange> removeDataPropertyAssertions(OWLNamedIndividual individual,
                                                                         Assertion assertion, Set<Value<?>> values) {
        final OWLDataProperty dataProperty = dataFactory.getOWLDataProperty(IRI.create(assertion.getIdentifier()));
        return values.stream().map(value -> {
            final OWLLiteral literal = OwlapiUtils
                    .createOWLLiteralFromValue(value.getValue(),
                            OwlapiUtils.getAssertionLanguage(assertion));
            return new MutableRemoveAxiom(ontology,
                    dataFactory.getOWLDataPropertyAssertionAxiom(dataProperty, individual, literal));
        }).collect(Collectors.toList());
    }

    private Collection<TransactionalChange> removeObjectPropertyAssertions(OWLNamedIndividual individual,
                                                                           Assertion assertion, Set<Value<?>> values) {
        final OWLObjectProperty objProperty = dataFactory.getOWLObjectProperty(IRI.create(assertion.getIdentifier()));
        return values.stream().map(value -> {
            final OWLIndividual object = OwlapiUtils
                    .getIndividual(NamedResource.create(value.stringValue()), dataFactory);
            return new MutableRemoveAxiom(ontology,
                    dataFactory.getOWLObjectPropertyAssertionAxiom(objProperty, individual, object));
        }).collect(Collectors.toList());
    }

    private Collection<TransactionalChange> removeAnnotationAssertions(OWLNamedIndividual individual,
                                                                       Assertion assertion, Set<Value<?>> values) {
        final OWLAnnotationProperty annProperty = dataFactory
                .getOWLAnnotationProperty(IRI.create(assertion.getIdentifier()));
        return values.stream().map(value -> {
            OWLAnnotationValue av;
            try {
                av = IRI.create(value.stringValue());
            } catch (IllegalArgumentException e) {
                av = OwlapiUtils.createOWLLiteralFromValue(value.getValue(),
                        OwlapiUtils.getAssertionLanguage(assertion));
            }
            return new MutableRemoveAxiom(ontology,
                    dataFactory.getOWLAnnotationAssertionAxiom(annProperty, individual.getIRI(), av));
        }).collect(Collectors.toList());
    }
}

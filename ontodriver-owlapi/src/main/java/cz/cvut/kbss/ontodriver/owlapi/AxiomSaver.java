package cz.cvut.kbss.ontodriver.owlapi;

import cz.cvut.kbss.ontodriver.owlapi.connector.OntologyStructures;
import cz.cvut.kbss.ontodriver.owlapi.util.MutableAddAxiom;
import cz.cvut.kbss.ontodriver.owlapi.util.OwlapiUtils;
import cz.cvut.kbss.ontodriver_new.descriptors.AxiomValueDescriptor;
import cz.cvut.kbss.ontodriver_new.model.Assertion;
import cz.cvut.kbss.ontodriver_new.model.NamedResource;
import cz.cvut.kbss.ontodriver_new.model.Value;
import org.semanticweb.owlapi.model.*;

import java.net.URI;
import java.net.URL;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Persists axioms into the ontology snapshot.
 */
class AxiomSaver {

    private final OwlapiAdapter adapter;

    private OWLOntology ontology;
    private OWLDataFactory dataFactory;
    private OWLOntologyManager ontologyManager;

    private String language;

    AxiomSaver(OwlapiAdapter adapter, OntologyStructures snapshot, String language) {
        this.adapter = adapter;
        this.ontology = snapshot.getOntology();
        this.dataFactory = snapshot.getDataFactory();
        this.ontologyManager = snapshot.getOntologyManager();
        this.language = language;
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

    private OWLNamedIndividual individual(NamedResource subject) {
        return dataFactory.getOWLNamedIndividual(IRI.create(subject.getIdentifier()));
    }

    private void persistDataPropertyValues(NamedResource subject, Assertion assertion, List<Value<?>> values) {
        final OWLNamedIndividual individual = individual(subject);
        final OWLDataProperty property = dataFactory.getOWLDataProperty(IRI.create(assertion.getIdentifier()));
        final List<OWLDataPropertyAssertionAxiom> axioms = values.stream().filter(value -> value != Value.nullValue())
                                                                 .map(
                                                                         value -> {
                                                                             final OWLLiteral val = OwlapiUtils
                                                                                     .createOWLLiteralFromValue(
                                                                                             value.getValue(),
                                                                                             dataFactory, language);
                                                                             return dataFactory
                                                                                     .getOWLDataPropertyAssertionAxiom(
                                                                                             property, individual, val);
                                                                         }).collect(Collectors.toList());
        addAxioms(axioms);
    }

    private void addAxioms(List<? extends OWLAxiom> axioms) {
        if (axioms.isEmpty()) {
            return;
        }
        final List<OWLOntologyChange> changes = axioms.stream().map(axiom -> new MutableAddAxiom(ontology, axiom))
                                                      .collect(Collectors.toList());
        adapter.addTransactionalChanges(ontologyManager.applyChanges(changes));
    }

    private void persistAnnotationPropertyValues(NamedResource subject, Assertion assertion, List<Value<?>> values) {
        final IRI annotationSubject = IRI.create(subject.getIdentifier());
        final OWLAnnotationProperty property = dataFactory.getOWLAnnotationProperty(
                IRI.create(assertion.getIdentifier()));
        final List<OWLAnnotationAssertionAxiom> axioms = values.stream().filter(value -> value != Value.nullValue())
                                                               .map(
                                                                       value -> {
                                                                           final OWLAnnotationValue val =
                                                                                   isIri(value.getValue()) ? IRI.create(
                                                                                           value.stringValue()) :
                                                                                   OwlapiUtils
                                                                                           .createOWLLiteralFromValue(
                                                                                                   value.getValue(),
                                                                                                   dataFactory,
                                                                                                   language);
                                                                           return dataFactory
                                                                                   .getOWLAnnotationAssertionAxiom(
                                                                                           property, annotationSubject,
                                                                                           val);
                                                                       }).collect(Collectors.toList());
        addAxioms(axioms);
    }

    private boolean isIri(Object value) {
        // Maybe a more sophisticated check would be better?
        return value instanceof URI || value instanceof URL;
    }

    private void persistObjectPropertyValues(NamedResource subject, Assertion assertion, List<Value<?>> values) {
        final OWLNamedIndividual individual = individual(subject);
        final OWLObjectProperty property = dataFactory.getOWLObjectProperty(IRI.create(assertion.getIdentifier()));
        final List<OWLObjectPropertyAssertionAxiom> axioms = values.stream().filter(value -> value != Value.nullValue())
                                                                   .map(
                                                                           value -> {
                                                                               // Simplistic version using value.stringValue
                                                                               // We expect the value to  be a NamedResource, but in case the property was unspecified and it was only assumed
                                                                               // it is an object property (see #persistPropertyValues), the value would be a simple string
                                                                               final OWLNamedIndividual target = dataFactory
                                                                                       .getOWLNamedIndividual(
                                                                                               IRI.create(
                                                                                                       value.stringValue()));
                                                                               return dataFactory
                                                                                       .getOWLObjectPropertyAssertionAxiom(
                                                                                               property, individual,
                                                                                               target);
                                                                           }).collect(Collectors.toList());
        addAxioms(axioms);
    }

    private void persistPropertyValues(NamedResource subject, Assertion assertion, List<Value<?>> values) {
        final IRI property = IRI.create(assertion.getIdentifier());
        if (ontology.containsDataPropertyInSignature(property)) {
            persistDataPropertyValues(subject, assertion, values);
        } else if (ontology.containsAnnotationPropertyInSignature(property)) {
            persistAnnotationPropertyValues(subject, assertion, values);
        } else {
            // The property is: 1) known to be an object property, 2) unknown, we just try object property
            persistObjectPropertyValues(subject, assertion, values);
        }
    }
}

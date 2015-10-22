package cz.cvut.kbss.ontodriver.owlapi;

import cz.cvut.kbss.ontodriver.owlapi.connector.OntologyStructures;
import cz.cvut.kbss.ontodriver.owlapi.util.MutableRemoveAxiom;
import cz.cvut.kbss.ontodriver.owlapi.util.OwlapiUtils;
import cz.cvut.kbss.ontodriver_new.descriptors.AxiomDescriptor;
import cz.cvut.kbss.ontodriver_new.model.Assertion;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.search.EntitySearcher;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

class EpistemicAxiomRemover {

    private final OwlapiAdapter owlapiAdapter;
    private final OWLOntology ontology;
    private final OWLOntologyManager ontologyManager;
    private final OWLDataFactory dataFactory;

    EpistemicAxiomRemover(OwlapiAdapter adapter, OntologyStructures snapshot) {
        this.owlapiAdapter = adapter;
        this.ontology = snapshot.getOntology();
        this.ontologyManager = snapshot.getOntologyManager();
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
            owlapiAdapter.addTransactionalChanges(ontologyManager.applyChanges(changes));
        }
    }

    private Collection<OWLOntologyChange> removeClassAssertionAxioms(OWLNamedIndividual individual) {
        final Collection<OWLClassExpression> types = EntitySearcher.getTypes(individual, ontology);
        return types.stream().map(cls -> new MutableRemoveAxiom(ontology,
                dataFactory.getOWLClassAssertionAxiom(cls, individual))).collect(Collectors.toList());
    }

    private Collection<? extends OWLOntologyChange> removeDataPropertyAssertions(OWLNamedIndividual individual,
                                                                                 Assertion assertion) {
        final OWLDataProperty dataProperty = dataFactory.getOWLDataProperty(IRI.create(assertion.getIdentifier()));
        final Collection<OWLLiteral> values = EntitySearcher.getDataPropertyValues(individual, dataProperty, ontology);
        return values.stream().map(value -> new MutableRemoveAxiom(ontology,
                dataFactory.getOWLDataPropertyAssertionAxiom(dataProperty, individual, value)))
                     .collect(Collectors.toList());
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

    private Collection<? extends OWLOntologyChange> removeAnnotationAssertions(OWLNamedIndividual individual,
                                                                               Assertion assertion) {
        final OWLAnnotationProperty annProperty = dataFactory
                .getOWLAnnotationProperty(IRI.create(assertion.getIdentifier()));
        final Collection<OWLAnnotationAssertionAxiom> values = EntitySearcher
                .getAnnotationAssertionAxioms(individual.getIRI(), ontology);
        return values.stream().filter(axiom -> axiom.getProperty().equals(annProperty))
                     .map(value -> new MutableRemoveAxiom(ontology, value)).collect(Collectors.toList());
    }
}

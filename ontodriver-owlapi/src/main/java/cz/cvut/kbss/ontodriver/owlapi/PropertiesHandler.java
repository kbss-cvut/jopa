package cz.cvut.kbss.ontodriver.owlapi;

import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver.owlapi.connector.OntologyStructures;
import cz.cvut.kbss.ontodriver_new.model.Assertion;
import cz.cvut.kbss.ontodriver_new.model.Axiom;
import cz.cvut.kbss.ontodriver_new.model.NamedResource;
import cz.cvut.kbss.ontodriver_new.model.Value;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.reasoner.OWLReasoner;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;
import java.util.Set;

class PropertiesHandler {

    private OWLOntology ontology;
    private OWLDataFactory dataFactory;
    private OWLReasoner reasoner;
    private OWLOntologyManager ontologyManager;

    private OwlapiAdapter adapter;

    private final AxiomAdapter axiomAdapter;

    PropertiesHandler(OwlapiAdapter adapter, OntologyStructures snapshot) {
        this.ontology = snapshot.getOntology();
        this.dataFactory = snapshot.getDataFactory();
        this.reasoner = snapshot.getReasoner();
        this.ontologyManager = snapshot.getOntologyManager();
        this.adapter = adapter;
        this.axiomAdapter = new AxiomAdapter(dataFactory, adapter.getLanguage());
    }

    public Collection<Axiom<?>> getProperties(NamedResource subject, boolean includeInferred)
            throws OntoDriverException {
        if (includeInferred) {
            return getPropertiesIncludingInferred(subject);
        } else {
            return getExplicitProperties(subject);
        }
    }

    private Collection<Axiom<?>> getPropertiesIncludingInferred(NamedResource subject) {
        final Collection<Axiom<?>> axioms = new ArrayList<>();
        final OWLNamedIndividual individual = getIndividual(subject);
        for (OWLDataProperty dp : ontology.getDataPropertiesInSignature()) {
            final Set<OWLLiteral> values = reasoner.getDataPropertyValues(individual, dp);
            for (OWLLiteral literal : values) {
                axioms.add(axiomAdapter.createAxiom(subject,
                        Assertion.createDataPropertyAssertion(dp.getIRI().toURI(), true), literal));
            }
        }
        for (OWLObjectProperty op : ontology.getObjectPropertiesInSignature()) {
            final Set<OWLNamedIndividual> values = reasoner.getObjectPropertyValues(individual, op).getFlattened();
            for (OWLNamedIndividual ind : values) {
                axioms.add(axiomAdapter.createAxiom(subject,
                        Assertion.createObjectPropertyAssertion(op.getIRI().toURI(), true), NamedResource.create(
                                ind.getIRI().toURI())));
            }
        }
        return axioms;
    }

    private Collection<Axiom<?>> getExplicitProperties(NamedResource subject) {
        final OWLNamedIndividual individual = getIndividual(subject);
        final Collection<Axiom<?>> axioms = new ArrayList<>();
        ontology.getDataPropertyAssertionAxioms(individual)
                .forEach(assertion -> axioms.add(axiomAdapter.toAxiom(subject, assertion, false)));
        ontology.getObjectPropertyAssertionAxioms(individual)
                .forEach(assertion -> axioms.add(axiomAdapter.toAxiom(subject, assertion, false)));
        ontology.getAnnotationAssertionAxioms(individual.getIRI())
                .forEach(assertion -> axioms.add(axiomAdapter.toAxiom(subject, assertion, false)));
        return axioms;
    }

    private OWLNamedIndividual getIndividual(NamedResource subject) {
        return dataFactory.getOWLNamedIndividual(IRI.create(subject.getIdentifier()));
    }

    public void addProperties(NamedResource subject, Map<Assertion, Set<Value<?>>> properties)
            throws OntoDriverException {
        final OntologyStructures snapshot = new OntologyStructures(ontology, ontologyManager, dataFactory, reasoner);
        new AxiomSaver(adapter, snapshot).persistAxioms(subject, properties);
    }

    public void removeProperties(NamedResource subject, Map<Assertion, Set<Value<?>>> properties)
            throws OntoDriverException {

    }
}

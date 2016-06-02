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

    ContextDefinition(String name) {
        this.name = name;
    }

    final Set<OWLAxiom> axioms = new HashSet<>();

    final IntegrityConstraintParserImpl parser = new IntegrityConstraintParserImpl(
            OWLManager.getOWLDataFactory(), this);
}
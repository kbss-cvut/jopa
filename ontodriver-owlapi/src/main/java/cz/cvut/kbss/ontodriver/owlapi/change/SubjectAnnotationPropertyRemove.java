package cz.cvut.kbss.ontodriver.owlapi.change;

import org.semanticweb.owlapi.model.OWLAnnotationAssertionAxiom;
import org.semanticweb.owlapi.model.OWLAnnotationProperty;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyChange;
import org.semanticweb.owlapi.model.RemoveAxiom;
import org.semanticweb.owlapi.search.EntitySearcher;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class SubjectAnnotationPropertyRemove extends SubjectPropertyRemove<OWLAnnotationProperty> {

    public SubjectAnnotationPropertyRemove(OWLNamedIndividual subject, OWLAnnotationProperty property) {
        super(subject, property);
    }

    @Override
    public List<OWLOntologyChange> toOwlChanges(OWLOntology targetOntology) {
        final Stream<OWLAnnotationAssertionAxiom> values =
                EntitySearcher.getAnnotationAssertionAxioms(subject.getIRI(), targetOntology);
        return values.filter(axiom -> axiom.getProperty().equals(property))
                     .map(value -> new RemoveAxiom(targetOntology, value)).collect(Collectors.toList());
    }
}

package cz.cvut.kbss.ontodriver.owlapi.change;

import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLIndividual;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLObjectProperty;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyChange;
import org.semanticweb.owlapi.model.RemoveAxiom;
import org.semanticweb.owlapi.search.EntitySearcher;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class SubjectObjectPropertyRemove extends SubjectPropertyRemove<OWLObjectProperty> {

    public SubjectObjectPropertyRemove(OWLNamedIndividual subject, OWLObjectProperty property) {
        super(subject, property);
    }

    @Override
    public List<OWLOntologyChange> toOwlChanges(OWLOntology targetOntology, OWLDataFactory dataFactory) {
        final Stream<OWLIndividual> values = EntitySearcher.getObjectPropertyValues(subject, property, targetOntology);
        return values.filter(OWLIndividual::isNamed).map(value -> new RemoveAxiom(targetOntology,
                             dataFactory.getOWLObjectPropertyAssertionAxiom(property, subject, value)))
                     .collect(Collectors.toList());
    }
}

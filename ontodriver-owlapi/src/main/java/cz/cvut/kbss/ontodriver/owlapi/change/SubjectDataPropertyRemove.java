package cz.cvut.kbss.ontodriver.owlapi.change;

import org.semanticweb.owlapi.model.OWLDataProperty;
import org.semanticweb.owlapi.model.OWLLiteral;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyChange;
import org.semanticweb.owlapi.model.RemoveAxiom;
import org.semanticweb.owlapi.search.EntitySearcher;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class SubjectDataPropertyRemove extends SubjectPropertyRemove<OWLDataProperty> {

    public SubjectDataPropertyRemove(OWLNamedIndividual subject, OWLDataProperty property) {
        super(subject, property);
    }

    @Override
    public List<OWLOntologyChange> toOwlChanges(OWLOntology targetOntology) {
        final Stream<OWLLiteral> values = EntitySearcher.getDataPropertyValues(subject, property, targetOntology);
        return values.map(value -> new RemoveAxiom(targetOntology, targetOntology.getOWLOntologyManager()
                                                                                 .getOWLDataFactory()
                                                                                 .getOWLDataPropertyAssertionAxiom(property, subject, value)))
                     .collect(Collectors.toList());
    }
}

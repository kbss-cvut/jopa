package cz.cvut.kbss.ontodriver.owlapi.change;

import org.semanticweb.owlapi.model.AxiomType;
import org.semanticweb.owlapi.model.OWLClassAssertionAxiom;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyChange;
import org.semanticweb.owlapi.model.RemoveAxiom;
import org.semanticweb.owlapi.search.EntitySearcher;

import java.util.List;
import java.util.stream.Collectors;

public class SubjectClassAssertionRemove implements TransactionalChange {

    private final OWLNamedIndividual subject;

    public SubjectClassAssertionRemove(OWLNamedIndividual subject) {
        this.subject = subject;
    }

    @Override
    public List<OWLOntologyChange> toOwlChanges(OWLOntology targetOntology) {
        return EntitySearcher.getTypes(subject, targetOntology).map(cls -> new RemoveAxiom(targetOntology,
                                     targetOntology.getOWLOntologyManager().getOWLDataFactory().getOWLClassAssertionAxiom(cls, subject)))
                             .collect(Collectors.toList());
    }

    @Override
    public boolean overrides(TransactionalChange existing) {
        if (existing instanceof MutableAddAxiom) {
            final MutableAddAxiom ax = (MutableAddAxiom) existing;
            return ax.getAxiom()
                     .isOfType(AxiomType.CLASS_ASSERTION) && subject.equals(((OWLClassAssertionAxiom) ax.getAxiom()).getIndividual());
        }
        return false;
    }
}

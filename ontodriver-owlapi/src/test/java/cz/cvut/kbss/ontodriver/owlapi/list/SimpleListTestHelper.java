package cz.cvut.kbss.ontodriver.owlapi.list;

import cz.cvut.kbss.jopa.model.SequencesVocabulary;
import cz.cvut.kbss.ontodriver.owlapi.connector.OntologyStructures;
import org.semanticweb.owlapi.model.*;

import java.net.URI;
import java.util.List;

class SimpleListTestHelper extends ListTestHelper {

    SimpleListTestHelper(OntologyStructures snapshot, OWLNamedIndividual individual) {
        super(snapshot, individual);
    }

    @Override
    void persistList(List<URI> items) {
        assert items.size() > 0;
        final OWLObjectProperty hasList = dataFactory
                .getOWLObjectProperty(IRI.create(SequencesVocabulary.s_p_hasListProperty));
        final OWLObjectProperty hasNext = dataFactory.getOWLObjectProperty(IRI.create(SequencesVocabulary.s_p_hasNext));
        manager.addAxiom(ontology, dataFactory.getOWLObjectPropertyAssertionAxiom(hasList, individual,
                dataFactory.getOWLNamedIndividual(IRI.create(items.get(0)))));
        for (int i = 1; i < items.size(); i++) {
            manager.addAxiom(ontology,
                    dataFactory.getOWLObjectPropertyAssertionAxiom(hasNext, dataFactory.getOWLNamedIndividual(
                                    IRI.create(items.get(i - 1))),
                            dataFactory.getOWLNamedIndividual(IRI.create(items.get(i)))));

        }
    }
}

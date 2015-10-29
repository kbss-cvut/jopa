package cz.cvut.kbss.ontodriver.owlapi.list;

import cz.cvut.kbss.jopa.model.SequencesVocabulary;
import cz.cvut.kbss.ontodriver.owlapi.connector.OntologySnapshot;
import org.semanticweb.owlapi.model.*;

import java.net.URI;
import java.util.List;

public class ReferencedListTestHelper extends ListTestHelper {

    static String SEQUENCE_NODE_SUFFIX = "-SEQ_";

    private final String baseUri;

    ReferencedListTestHelper(OntologySnapshot snapshot, OWLNamedIndividual individual, String baseUri) {
        super(snapshot, individual);
        this.baseUri = baseUri;
    }

    @Override
    public void persistList(List<URI> items) {
        assert items.size() > 0;
        final OWLObjectProperty hasList = dataFactory.getOWLObjectProperty(
                IRI.create(SequencesVocabulary.s_p_hasListProperty));
        final OWLObjectProperty hasNext = dataFactory.getOWLObjectProperty(IRI.create(SequencesVocabulary.s_p_hasNext));
        final OWLObjectProperty hasContent = dataFactory.getOWLObjectProperty(
                IRI.create(SequencesVocabulary.s_p_hasContents));
        int i = 0;
        final String sequenceNodeBase = baseUri + "-SEQ_";
        OWLNamedIndividual node = dataFactory.getOWLNamedIndividual(IRI.create(sequenceNodeBase + i));
        manager.addAxiom(ontology, dataFactory.getOWLObjectPropertyAssertionAxiom(hasList, individual, node));
        manager.addAxiom(ontology, dataFactory.getOWLObjectPropertyAssertionAxiom(hasContent, node,
                dataFactory.getOWLNamedIndividual(IRI.create(items.get(i)))));
        OWLNamedIndividual previousNode;
        for (i = 1; i < items.size(); i++) {
            previousNode = node;
            node = dataFactory.getOWLNamedIndividual(IRI.create(sequenceNodeBase + i));
            manager.addAxiom(ontology,
                    dataFactory.getOWLObjectPropertyAssertionAxiom(hasNext, previousNode, node));
            manager.addAxiom(ontology, dataFactory.getOWLObjectPropertyAssertionAxiom(hasContent, node,
                    dataFactory.getOWLNamedIndividual(IRI.create(items.get(i)))));

        }
    }
}

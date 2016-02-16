package cz.cvut.kbss.ontodriver.owlapi.list;

import cz.cvut.kbss.ontodriver.owlapi.connector.OntologySnapshot;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLObjectProperty;

import java.net.URI;
import java.util.List;

class SimpleListTestHelper extends ListTestHelper {

    SimpleListTestHelper(OntologySnapshot snapshot, OWLNamedIndividual individual) {
        super(snapshot, individual);
    }

    @Override
    void persistList(List<URI> items) {
        assert items.size() > 0;
        final OWLObjectProperty hasList = dataFactory
                .getOWLObjectProperty(IRI.create(HAS_LIST_PROPERTY));
        final OWLObjectProperty hasNext = dataFactory.getOWLObjectProperty(IRI.create(HAS_NEXT_PROPERTY));
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

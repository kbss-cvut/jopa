package cz.cvut.kbss.ontodriver.owlapi.list;

import cz.cvut.kbss.ontodriver.owlapi.connector.OntologySnapshot;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyManager;

import java.net.URI;
import java.util.List;

abstract class ListTestHelper {

    final OWLOntology ontology;
    final OWLOntologyManager manager;
    final OWLDataFactory dataFactory;
    final OWLNamedIndividual individual;

    ListTestHelper(OntologySnapshot snapshot, OWLNamedIndividual individual) {
        this.ontology = snapshot.getOntology();
        this.manager = snapshot.getOntologyManager();
        this.dataFactory = snapshot.getDataFactory();
        this.individual = individual;
    }

    abstract void persistList(List<URI> items);
}

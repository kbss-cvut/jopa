package cz.cvut.kbss.ontodriver.owlapi.list;

import cz.cvut.kbss.ontodriver.owlapi.connector.OntologySnapshot;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyManager;

import java.net.URI;
import java.util.List;

abstract class ListTestHelper {

    public static String HAS_LIST_PROPERTY = "http://krizik.felk.cvut.cz/ontologies/2008/6/sequences.owl#hasListProperty";
    public static String HAS_NEXT_PROPERTY = "http://krizik.felk.cvut.cz/ontologies/2008/6/sequences.owl#hasNext";
    public static String HAS_CONTENT_PROPERTY = "http://krizik.felk.cvut.cz/ontologies/2008/6/sequences.owl#hasContent";

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

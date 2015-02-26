package cz.cvut.kbss.ontodriver.owlapi.connector;

import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyManager;

/**
 * Created by ledvima1 on 26.2.15.
 */
public class OntologyStructures {

    private final OWLOntology ontology;
    private final OWLOntologyManager ontologyManager;
    private final OWLDataFactory dataFactory;

    public OntologyStructures(OWLOntology ontology, OWLOntologyManager ontologyManager, OWLDataFactory dataFactory) {
        this.ontology = ontology;
        this.ontologyManager = ontologyManager;
        this.dataFactory = dataFactory;
    }

    public OWLOntology getOntology() {
        return ontology;
    }

    public OWLOntologyManager getOntologyManager() {
        return ontologyManager;
    }

    public OWLDataFactory getDataFactory() {
        return dataFactory;
    }
}

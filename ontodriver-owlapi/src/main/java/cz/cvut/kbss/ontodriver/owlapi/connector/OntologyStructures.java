package cz.cvut.kbss.ontodriver.owlapi.connector;

import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.reasoner.OWLReasoner;

/**
 * Created by ledvima1 on 26.2.15.
 */
public class OntologyStructures {

    private final OWLOntology ontology;
    private final OWLOntologyManager ontologyManager;
    private final OWLDataFactory dataFactory;
    private final OWLReasoner reasoner;

    public OntologyStructures(OWLOntology ontology, OWLOntologyManager ontologyManager, OWLDataFactory dataFactory, OWLReasoner reasoner) {
        this.ontology = ontology;
        this.ontologyManager = ontologyManager;
        this.dataFactory = dataFactory;
        this.reasoner = reasoner;
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

    public OWLReasoner getReasoner() {
        return reasoner;
    }
}

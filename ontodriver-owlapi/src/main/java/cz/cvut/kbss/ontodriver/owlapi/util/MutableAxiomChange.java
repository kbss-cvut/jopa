package cz.cvut.kbss.ontodriver.owlapi.util;

import org.semanticweb.owlapi.model.OWLOntology;

/**
 * Interface which enables to set owner ontology of axiom changes.
 */
public interface MutableAxiomChange {

    /**
     * Sets ontology on this object. </p>
     *
     * @param ontology The ontology to set
     */
    public void setOntology(OWLOntology ontology);

    /**
     * Retrieves this object's current ontology;
     *
     * @return OWLOntology
     */
    public OWLOntology getOntology();
}

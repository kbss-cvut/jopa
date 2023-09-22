package cz.cvut.kbss.ontodriver.owlapi.change;

import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyChange;

import java.util.List;

public interface TransactionalChange {

    List<OWLOntologyChange> toOwlChanges(OWLOntology targetOntology, OWLDataFactory dataFactory);

    default boolean overrides(TransactionalChange existing) {return false;}
}

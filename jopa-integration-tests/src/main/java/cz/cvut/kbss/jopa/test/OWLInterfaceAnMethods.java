package cz.cvut.kbss.jopa.test;

import cz.cvut.kbss.jopa.model.annotations.OWLAnnotationProperty;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;

@OWLClass(iri = Vocabulary.C_OwlInterfaceAnMethods)
public interface OWLInterfaceAnMethods {
    @OWLDataProperty(iri="best_iri_ever")
    void setName(String name);
}

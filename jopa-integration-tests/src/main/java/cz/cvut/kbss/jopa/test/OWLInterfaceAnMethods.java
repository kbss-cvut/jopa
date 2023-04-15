package cz.cvut.kbss.jopa.test;

import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;

@OWLClass(iri = Vocabulary.C_OwlInterfaceAnMethods)
public interface OWLInterfaceAnMethods {
    @OWLDataProperty(iri=Vocabulary.p_m_unannotated_name)
    void setName(String name);

    String getName();
}

package cz.cvut.kbss.jopa.test;

import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;

@OWLClass(iri = Vocabulary.C_OWL_INTERFACE_A)
public interface OWLInterfaceA {
    @OWLDataProperty(iri = Vocabulary.p_m_attributeA)
    String getAttributeA();

    void setAttributeA(String attribute);
}

package cz.cvut.kbss.jopa.test;

import cz.cvut.kbss.jopa.model.annotations.OWLAnnotationProperty;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;

@OWLClass(iri = Vocabulary.C_OWL_INTERFACE_B)
public interface OWLInterfaceB {
    @OWLAnnotationProperty(iri = Vocabulary.p_m_attributeB)
    void setAttributeB(Boolean attr);

    Boolean getAttributeB();
}

package cz.cvut.kbss.jopa.test;

import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;
import cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty;

@OWLClass(iri = Vocabulary.C_OWL_CLASS_T)
public class OWLClassT extends OWLClassS {

    @OWLDataProperty(iri = Vocabulary.P_T_INTEGER_ATTRIBUTE)
    private Integer intAttribute;

    @OWLObjectProperty(iri = Vocabulary.P_HAS_OWL_CLASS_A)
    private OWLClassA owlClassA;

    public Integer getIntAttribute() {
        return intAttribute;
    }

    public void setIntAttribute(Integer intAttribute) {
        this.intAttribute = intAttribute;
    }

    public OWLClassA getOwlClassA() {
        return owlClassA;
    }

    public void setOwlClassA(OWLClassA owlClassA) {
        this.owlClassA = owlClassA;
    }

}

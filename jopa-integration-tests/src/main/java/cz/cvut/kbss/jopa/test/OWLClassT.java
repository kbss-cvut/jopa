package cz.cvut.kbss.jopa.test;

import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;
import cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty;
import cz.cvut.kbss.jopa.model.annotations.Types;

import java.util.Set;

@OWLClass(iri = Vocabulary.cOWLClassT)
public class OWLClassT extends OWLClassS {

    @OWLDataProperty(iri = Vocabulary.tIntegerAttribute)
    private Integer intAttribute;

    @OWLObjectProperty(iri = Vocabulary.hasOwlClassA)
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

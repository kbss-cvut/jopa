package cz.cvut.kbss.jopa.test;

import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty;

@OWLClass(iri = Vocabulary.C_OWL_CLASS_U)
public class OWLClassU extends OWLClassS {

    // Polymorphic attribute
    @OWLObjectProperty(iri = Vocabulary.P_HAS_OWL_CLASS_S)
    private OWLClassS owlClassS;

    public OWLClassS getOwlClassS() {
        return owlClassS;
    }

    public void setOwlClassS(OWLClassS owlClassS) {
        this.owlClassS = owlClassS;
    }
}

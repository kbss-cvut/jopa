package cz.cvut.kbss.jopa.test;

import cz.cvut.kbss.jopa.model.annotations.OWLClass;

@OWLClass(iri = Vocabulary.C_ChildOfMappedSuperClass)
public class ChildOfMappedSuperClass extends MappedSuperClassWithAnnotatedMethods {
    private String label;

    @Override
    public String getLabel() {
        return label;
    }

    public void setLabel(String label) {
        this.label = label;
    }
}

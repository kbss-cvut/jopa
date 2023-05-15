package cz.cvut.kbss.jopa.test;

import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;

import java.net.URI;

@OWLClass(iri = Vocabulary.C_OWL_CLASS_CHILD_B)
public class OWLChildClassB implements OWLInterfaceA, OWLInterfaceB {
    private String attributeA;

    private Boolean attributeB;
    @Id
    private URI id;

    public URI getId() {
        return id;
    }

    public void setId(URI id) {
        this.id = id;
    }

    @Override
    public String getAttributeA() {
        return attributeA;
    }

    @Override
    public void setAttributeB(Boolean attr) {
        attributeB = attr;
    }

    @Override
    public void setAttributeA(String attributeA) {
        this.attributeA = attributeA;
    }

    @Override
    public Boolean getAttributeB() {
        return attributeB;
    }
}

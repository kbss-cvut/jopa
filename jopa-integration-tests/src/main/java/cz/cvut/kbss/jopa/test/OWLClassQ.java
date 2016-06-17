package cz.cvut.kbss.jopa.test;

import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;

import java.lang.reflect.Field;

@OWLClass(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassQ")
public class OWLClassQ extends QMappedSuperclass {

    @OWLDataProperty(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#Q-stringAttribute")
    private String stringAttribute;

    public String getStringAttribute() {
        return stringAttribute;
    }

    public void setStringAttribute(String stringAttribute) {
        this.stringAttribute = stringAttribute;
    }

    @Override
    public String toString() {
        return "OWLClassQ{" +
                "stringAttribute='" + stringAttribute + '\'' +
                "} " + super.toString();
    }

    public static Field getOWlClassAField() throws Exception {
        return QMappedSuperclass.class.getDeclaredField("owlClassA");
    }
}

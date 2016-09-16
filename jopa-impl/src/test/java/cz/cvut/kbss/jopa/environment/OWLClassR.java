package cz.cvut.kbss.jopa.environment;

import cz.cvut.kbss.jopa.model.annotations.*;

import java.lang.reflect.Field;

@OWLClass(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassR")
public class OWLClassR extends OWLClassS {

    @OWLDataProperty(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#R-stringAttribute")
    private String stringAtt;

    @OWLObjectProperty(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#hasA", cascade = {CascadeType.PERSIST})
    private OWLClassA owlClassA;

    public String getStringAtt() {
        return stringAtt;
    }

    public void setStringAtt(String stringAtt) {
        this.stringAtt = stringAtt;
    }

    public OWLClassA getOwlClassA() {
        return owlClassA;
    }

    public void setOwlClassA(OWLClassA owlClassA) {
        this.owlClassA = owlClassA;
    }

    @Override
    public String toString() {
        return "OWLClassR{" +
                "owlClassA=" + owlClassA +
                ", stringAtt='" + stringAtt + '\'' +
                "} " + super.toString();
    }

    public static String getClassIri() {
        return OWLClassR.class.getDeclaredAnnotation(OWLClass.class).iri();
    }

    public static Field getStringAttField() throws NoSuchFieldException {
        return OWLClassR.class.getDeclaredField("stringAtt");
    }

    public static Field getOwlClassAField() throws NoSuchFieldException {
        return OWLClassR.class.getDeclaredField("owlClassA");
    }
}

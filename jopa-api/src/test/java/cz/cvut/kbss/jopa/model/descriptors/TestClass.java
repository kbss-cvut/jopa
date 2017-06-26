package cz.cvut.kbss.jopa.model.descriptors;

import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;

import java.lang.reflect.Field;

class TestClass {

    @OWLDataProperty(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes/stringAtt")
    private String stringAtt;

    @OWLDataProperty(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes/intAtt")
    private Integer intAtt;

    public String getStringAtt() {
        return stringAtt;
    }

    public void setStringAtt(String stringAtt) {
        this.stringAtt = stringAtt;
    }

    public Integer getIntAtt() {
        return intAtt;
    }

    public void setIntAtt(Integer intAtt) {
        this.intAtt = intAtt;
    }

    static Field stringAttField() throws NoSuchFieldException {
        return TestClass.class.getDeclaredField("stringAtt");
    }

    static Field intAttField() throws NoSuchFieldException {
        return TestClass.class.getDeclaredField("intAtt");
    }
}

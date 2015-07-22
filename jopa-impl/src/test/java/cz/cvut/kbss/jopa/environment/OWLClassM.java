package cz.cvut.kbss.jopa.environment;

import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;

import java.lang.reflect.Field;
import java.util.Date;

/**
 * Contains a generated string URI and data property attributes of primitive wrapper types
 * - boolean, int, long, double.
 *
 * @author ledvima1
 */
@OWLClass(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassM")
public class OWLClassM {

    @Id(generated = true)
    private String key;

    @OWLDataProperty(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#m-booleanAttribute")
    private Boolean booleanAttribute;

    @OWLDataProperty(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#m-intAttribute")
    private Integer intAttribute;

    @OWLDataProperty(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#m-longAttribute")
    private Long longAttribute;

    @OWLDataProperty(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#m-doubleAttribute")
    private Double doubleAttribute;

    @OWLDataProperty(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#m-dateAttribute")
    private Date dateAttribute;

    @OWLDataProperty(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#m-enumAttribute")
    private Severity enumAttribute;

    public enum Severity {
        LOW, MEDIUM, HIGH
    }

    public String getKey() {
        return key;
    }

    public void setKey(String key) {
        this.key = key;
    }

    public Boolean getBooleanAttribute() {
        return booleanAttribute;
    }

    public void setBooleanAttribute(Boolean booleanAttribute) {
        this.booleanAttribute = booleanAttribute;
    }

    public Integer getIntAttribute() {
        return intAttribute;
    }

    public void setIntAttribute(Integer intAttribute) {
        this.intAttribute = intAttribute;
    }

    public Long getLongAttribute() {
        return longAttribute;
    }

    public void setLongAttribute(Long longAttribute) {
        this.longAttribute = longAttribute;
    }

    public Double getDoubleAttribute() {
        return doubleAttribute;
    }

    public void setDoubleAttribute(Double doubleAttribute) {
        this.doubleAttribute = doubleAttribute;
    }

    public Date getDateAttribute() {
        return dateAttribute;
    }

    public void setDateAttribute(Date dateAttribute) {
        this.dateAttribute = dateAttribute;
    }

    public Severity getEnumAttribute() {
        return enumAttribute;
    }

    public void setEnumAttribute(Severity enumAttribute) {
        this.enumAttribute = enumAttribute;
    }

    @Override
    public String toString() {
        return "OWLCLassM{" +
                "key='" + key + '\'' +
                ", booleanAttribute=" + booleanAttribute +
                ", intAttribute=" + intAttribute +
                ", longAttribute=" + longAttribute +
                ", doubleAttribute=" + doubleAttribute +
                ", enumAttribute=" + enumAttribute +
                '}';
    }

    public void initializeTestValues(boolean includingKey) {
        if (includingKey) {
            this.key = "http://krizik.felk.cvut.cz/ontologies/entityM";
        }
        this.booleanAttribute = true;
        this.intAttribute = 117;
        this.longAttribute = 365L;
        this.doubleAttribute = 3.14D;
        this.dateAttribute = new Date();
        this.enumAttribute = Severity.MEDIUM;
    }

    public static String getClassIri() throws Exception {
        return OWLClassM.class.getAnnotation(OWLClass.class).iri();
    }

    public static Field getUriField() throws Exception {
        return OWLClassM.class.getDeclaredField("key");
    }

    public static Field getBooleanAttributeField() throws Exception {
        return OWLClassM.class.getDeclaredField("booleanAttribute");
    }

    public static Field getIntAttributeField() throws Exception {
        return OWLClassM.class.getDeclaredField("intAttribute");
    }

    public static Field getLongAttributeField() throws Exception {
        return OWLClassM.class.getDeclaredField("longAttribute");
    }

    public static Field getDoubleAttributeField() throws Exception {
        return OWLClassM.class.getDeclaredField("doubleAttribute");
    }

    public static Field getDateAttributeField() throws Exception {
        return OWLClassM.class.getDeclaredField("dateAttribute");
    }

    public static Field getEnumAttributeField() throws Exception {
        return OWLClassM.class.getDeclaredField("enumAttribute");
    }
}

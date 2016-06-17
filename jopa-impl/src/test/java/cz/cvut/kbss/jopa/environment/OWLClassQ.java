package cz.cvut.kbss.jopa.environment;

import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;

import java.lang.reflect.Field;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

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

    public static String getClassIri() {
        return OWLClassQ.class.getAnnotation(OWLClass.class).iri();
    }

    public static Field getUriField() throws Exception {
        return QMappedSuperclass.class.getDeclaredField("uri");
    }

    public static Field getStringAttributeField() throws Exception {
        return OWLClassQ.class.getDeclaredField("stringAttribute");
    }

    public static Field getLabelField() throws Exception {
        return QMappedSuperclass.class.getDeclaredField("label");
    }

    public static Field getParentStringField() throws Exception {
        return QMappedSuperclass.class.getDeclaredField("parentString");
    }

    public static Field getOwlClassAField() throws Exception {
        return QMappedSuperclass.class.getDeclaredField("owlClassA");
    }

    public static Set<Field> getPersistentFields() throws Exception {
        return new HashSet<>(
                Arrays.asList(getStringAttributeField(), getParentStringField(), getLabelField(), getOwlClassAField()));
    }
}

package cz.cvut.kbss.jopa.environment;

import cz.cvut.kbss.jopa.model.annotations.*;

import java.lang.reflect.Field;
import java.util.Map;
import java.util.Set;

/**
 * @author ledvima1
 */
@OWLClass(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassN")
public class OWLClassN {

    @Id(generated = true)
    private String id;

    @OWLAnnotationProperty(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#annotationProperty")
    private String annotationProperty;

    @ParticipationConstraints(nonEmpty = true)
    @OWLDataProperty(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#A-stringAttribute")
    private String stringAttribute;

    @Inferred
    @Properties(fetchType = FetchType.LAZY)
    private Map<String, Set<String>> properties;

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public String getAnnotationProperty() {
        return annotationProperty;
    }

    public void setAnnotationProperty(String annotationProperty) {
        this.annotationProperty = annotationProperty;
    }

    public String getStringAttribute() {
        return stringAttribute;
    }

    public void setStringAttribute(String stringAttribute) {
        this.stringAttribute = stringAttribute;
    }

    public Map<String, Set<String>> getProperties() {
        return properties;
    }

    public void setProperties(Map<String, Set<String>> properties) {
        this.properties = properties;
    }

    public static String getClassIri() throws Exception {
        return OWLClassN.class.getAnnotation(OWLClass.class).iri();
    }

    public static Field getAnnotationPropertyField() throws Exception {
        return OWLClassN.class.getDeclaredField("annotationProperty");
    }

    public static Field getStringAttributeField() throws Exception {
        return OWLClassN.class.getDeclaredField("stringAttribute");
    }

    public static Field getPropertiesField() throws Exception {
        return OWLClassN.class.getDeclaredField("properties");
    }
}

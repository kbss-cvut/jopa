package cz.cvut.kbss.jopa.environment;

import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty;
import cz.cvut.kbss.jopa.model.annotations.Properties;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.Map;
import java.util.Set;

@OWLClass(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassP")
public class OWLClassP {

    @Id(generated = true)
    private URI uri;

    @OWLObjectProperty(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#hasA")
    private URI individualUri;

    @Properties
    private Map<URI, Set<Object>> properties;

    public URI getUri() {
        return uri;
    }

    public void setUri(URI uri) {
        this.uri = uri;
    }

    public URI getIndividualUri() {
        return individualUri;
    }

    public void setIndividualUri(URI individualUri) {
        this.individualUri = individualUri;
    }

    public Map<URI, Set<Object>> getProperties() {
        return properties;
    }

    public void setProperties(Map<URI, Set<Object>> properties) {
        this.properties = properties;
    }

    public static String getClassIri() {
        return OWLClassP.class.getAnnotation(OWLClass.class).iri();
    }

    public static Field getUriField() throws Exception {
        return OWLClassP.class.getDeclaredField("uri");
    }

    public static Field getIndividualUriField() throws Exception {
        return OWLClassP.class.getDeclaredField("individualUri");
    }

    public static Field getPropertiesField() throws Exception {
        return OWLClassP.class.getDeclaredField("properties");
    }
}

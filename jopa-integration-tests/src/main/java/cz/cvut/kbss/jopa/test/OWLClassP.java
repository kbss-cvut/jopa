package cz.cvut.kbss.jopa.test;

import cz.cvut.kbss.jopa.model.annotations.FetchType;
import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.Properties;

import java.net.URI;
import java.util.Map;
import java.util.Set;

@OWLClass(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassP")
public class OWLClassP {

    @Id(generated = true)
    private URI uri;

    @Properties(fetchType = FetchType.EAGER)
    private Map<URI, Set<Object>> properties;

    public URI getUri() {
        return uri;
    }

    public void setUri(URI uri) {
        this.uri = uri;
    }

    public Map<URI, Set<Object>> getProperties() {
        return properties;
    }

    public void setProperties(Map<URI, Set<Object>> properties) {
        this.properties = properties;
    }

    @Override
    public String toString() {
        return "OWLClassP{" +
                "uri=" + uri +
                ", properties=" + properties +
                '}';
    }
}

package cz.cvut.kbss.jopa.test;

import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;

import java.net.URI;
import java.time.LocalDate;

@OWLClass(iri = "urn:jopa:model:class-with-urn")
public class OWLClassWithUrn implements HasUri {

    @Id
    private URI uri;

    @OWLDataProperty(iri = "urn:jopa:model:label")
    private String label;

    @OWLDataProperty(iri = "urn:jopa:model:created")
    private LocalDate created;

    @Override
    public URI getUri() {
        return uri;
    }

    public void setUri(URI uri) {
        this.uri = uri;
    }

    public String getLabel() {
        return label;
    }

    public void setLabel(String label) {
        this.label = label;
    }

    public LocalDate getCreated() {
        return created;
    }

    public void setCreated(LocalDate created) {
        this.created = created;
    }

    @Override
    public String toString() {
        return "OWLClassWithUrn{" +
                "uri=" + uri +
                ", label='" + label + '\'' +
                ", created=" + created +
                '}';
    }
}

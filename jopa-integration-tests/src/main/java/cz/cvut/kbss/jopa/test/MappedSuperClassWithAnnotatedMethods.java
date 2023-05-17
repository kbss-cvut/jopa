package cz.cvut.kbss.jopa.test;

import cz.cvut.kbss.jopa.model.annotations.*;
import cz.cvut.kbss.jopa.vocabulary.RDFS;

import java.net.URI;

@MappedSuperclass
public abstract class MappedSuperClassWithAnnotatedMethods {
    @Id(generated = true)
    private URI uri;
    @OWLAnnotationProperty(iri = RDFS.LABEL)

    public abstract String getLabel();

    public URI getUri() {
        return uri;
    }

    public void setUri(URI uri) {
        this.uri = uri;
    }
}



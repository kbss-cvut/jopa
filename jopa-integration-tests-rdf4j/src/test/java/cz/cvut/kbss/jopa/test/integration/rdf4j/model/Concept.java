package cz.cvut.kbss.jopa.test.integration.rdf4j.model;

import cz.cvut.kbss.jopa.model.annotations.*;
import cz.cvut.kbss.jopa.vocabulary.SKOS;

import java.net.URI;
import java.util.Set;

@OWLClass(iri = SKOS.CONCEPT)
public class Concept {

    @Id(generated = true)
    private URI uri;

    @OWLObjectProperty(iri = SKOS.BROADER, fetch = FetchType.EAGER)
    private Set<Concept> broader;

    @Inferred
    @OWLObjectProperty(iri = SKOS.NARROWER, fetch = FetchType.EAGER)
    private Set<Concept> narrower;

    public URI getUri() {
        return uri;
    }

    public void setUri(URI uri) {
        this.uri = uri;
    }

    public Set<Concept> getBroader() {
        return broader;
    }

    public void setBroader(Set<Concept> broader) {
        this.broader = broader;
    }

    public Set<Concept> getNarrower() {
        return narrower;
    }

    public void setNarrower(Set<Concept> narrower) {
        this.narrower = narrower;
    }

    @Override
    public String toString() {
        return "Concept{<" +
                uri +
                ">, broader=" + broader +
                ", narrower=" + narrower +
                '}';
    }
}

package cz.cvut.kbss.jopa.test;

import cz.cvut.kbss.jopa.model.annotations.*;

import java.net.URI;
import java.util.Set;

@OWLClass(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassO")
public class OWLClassO {

    @Id
    private URI uri;

    @OWLObjectProperty(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#hasE", cascade = {CascadeType.MERGE}, fetch = FetchType.EAGER)
    private Set<OWLClassE> owlClassESet;

    public OWLClassO() {
    }

    public OWLClassO(URI uri) {
        this.uri = uri;
    }

    public URI getUri() {
        return uri;
    }

    public void setUri(URI uri) {
        this.uri = uri;
    }

    public Set<OWLClassE> getOwlClassESet() {
        return owlClassESet;
    }

    public void setOwlClassESet(Set<OWLClassE> owlClassESet) {
        this.owlClassESet = owlClassESet;
    }

    @Override
    public String toString() {
        return "OWLClassO{" +
                "uri=" + uri +
                ", owlClassESet=" + owlClassESet +
                '}';
    }
}

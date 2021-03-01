package cz.cvut.kbss.jopa.test;

import cz.cvut.kbss.jopa.model.annotations.*;

import java.net.URI;

@OWLClass(iri = Vocabulary.C_OWL_CLASS_Z)
public class OWLClassZ {

    @Id
    private URI uri;

    @OWLObjectProperty(iri = Vocabulary.ATTRIBUTE_IRI_BASE + "hasRoot", cascade = CascadeType.ALL,fetch = FetchType.EAGER)
    private OWLClassZChild root;

    public URI getUri() {
        return uri;
    }

    public void setUri(URI uri) {
        this.uri = uri;
    }

    public OWLClassZChild getRoot() {
        return root;
    }

    public void setRoot(OWLClassZChild root) {
        this.root = root;
    }
}

package cz.cvut.kbss.jopa.test;

import cz.cvut.kbss.jopa.model.annotations.FetchType;
import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.Inferred;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;
import cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty;

import java.net.URI;
import java.util.Set;

@OWLClass(iri = Vocabulary.C_OWL_CLASS_F)
public class OWLClassFF implements HasUri {

    @Id
    private URI uri;

    @Inferred
    @OWLDataProperty(iri = Vocabulary.P_F_STRING_ATTRIBUTE)
    private String secondStringAttribute;

    @OWLObjectProperty(iri = Vocabulary.P_F_HAS_SIMPLE_SET, fetch = FetchType.EAGER)
    private Set<OWLClassA> simpleSet;

    public OWLClassFF() {
    }

    public OWLClassFF(URI uri) {
        this.uri = uri;
    }

    @Override
    public URI getUri() {
        return uri;
    }

    public void setUri(URI uri) {
        this.uri = uri;
    }

    public String getSecondStringAttribute() {
        return secondStringAttribute;
    }

    public void setSecondStringAttribute(String secondStringAttribute) {
        this.secondStringAttribute = secondStringAttribute;
    }

    public Set<OWLClassA> getSimpleSet() {
        return simpleSet;
    }

    public void setSimpleSet(Set<OWLClassA> simpleSet) {
        this.simpleSet = simpleSet;
    }

    @Override
    public String toString() {
        String out = "OWLClassFF: uri = " + uri;
        out += ", secondStringAttribute = " + secondStringAttribute;
        return out;
    }
}

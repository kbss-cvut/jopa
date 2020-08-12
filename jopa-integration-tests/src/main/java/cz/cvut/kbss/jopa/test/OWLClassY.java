package cz.cvut.kbss.jopa.test;

import cz.cvut.kbss.jopa.model.MultilingualString;
import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;

import java.io.Serializable;
import java.net.URI;
import java.util.Set;

@OWLClass(iri = Vocabulary.C_OWL_CLASS_Y)
public class OWLClassY implements Serializable {

    @Id(generated = true)
    private URI uri;

    @OWLDataProperty(iri = Vocabulary.P_Y_SINGULAR_MULTILINGUAL_ATTRIBUTE)
    private MultilingualString singularString;

    @OWLDataProperty(iri = Vocabulary.P_Y_PLURAL_MULTILINGUAL_ATTRIBUTE)
    private Set<MultilingualString> pluralString;

    public URI getUri() {
        return uri;
    }

    public void setUri(URI uri) {
        this.uri = uri;
    }

    public MultilingualString getSingularString() {
        return singularString;
    }

    public void setSingularString(MultilingualString singularString) {
        this.singularString = singularString;
    }

    public Set<MultilingualString> getPluralString() {
        return pluralString;
    }

    public void setPluralString(Set<MultilingualString> pluralString) {
        this.pluralString = pluralString;
    }

    @Override
    public String toString() {
        return "OWLClassY{" +
                "uri=" + uri +
                ", singularString=" + singularString +
                ", pluralString=" + pluralString +
                '}';
    }
}

package cz.cvut.kbss.jopa.environment;

import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;
import cz.cvut.kbss.jopa.model.annotations.util.NonEntity;

import java.net.URI;

@NonEntity
@OWLClass(iri = Vocabulary.c_OwlClassA)
public class NonPersistentClass {

    @Id(generated = true)
    private URI uri;

    @OWLDataProperty(iri = Vocabulary.p_a_stringAttribute)
    private String stringAttribute;

    public URI getUri() {
        return uri;
    }

    public void setUri(URI uri) {
        this.uri = uri;
    }

    public String getStringAttribute() {
        return stringAttribute;
    }

    public void setStringAttribute(String stringAttribute) {
        this.stringAttribute = stringAttribute;
    }
}

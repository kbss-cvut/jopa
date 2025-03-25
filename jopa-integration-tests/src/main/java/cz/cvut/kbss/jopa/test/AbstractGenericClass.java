package cz.cvut.kbss.jopa.test;

import cz.cvut.kbss.jopa.model.annotations.FetchType;
import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.MappedSuperclass;
import cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty;

import java.net.URI;
import java.util.Set;

@MappedSuperclass
public abstract class AbstractGenericClass<T extends HasUri> {

    @Id
    private URI uri;

    @OWLObjectProperty(iri = Vocabulary.ATTRIBUTE_IRI_BASE + "genericValue", fetch = FetchType.EAGER)
    private Set<T> genericValue;

    public URI getUri() {
        return uri;
    }

    public void setUri(URI uri) {
        this.uri = uri;
    }

    public Set<T> getGenericValue() {
        return genericValue;
    }

    public void setGenericValue(Set<T> genericValue) {
        this.genericValue = genericValue;
    }
}

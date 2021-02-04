package cz.cvut.kbss.jopa.test;

import cz.cvut.kbss.jopa.model.annotations.*;
import cz.cvut.kbss.jopa.vocabulary.RDFS;

import java.net.URI;
import java.util.HashSet;
import java.util.Objects;
import java.util.Set;

@OWLClass(iri = Vocabulary.C_OWL_CLASS_Z_CHILD)
public class OWLClassZChild {

    @Id
    private URI id;

    @ParticipationConstraints(nonEmpty = true)
    @OWLDataProperty(iri = RDFS.LABEL)
    private String name;

    @OWLObjectProperty(iri = Vocabulary.ATTRIBUTE_IRI_BASE + "hasChild", cascade = CascadeType.ALL, fetch = FetchType.EAGER)
    private Set<OWLClassZChild> children = new HashSet<>();

    public URI getId() {
        return id;
    }

    public void setId(URI id) {
        this.id = id;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public Set<OWLClassZChild> getChildren() {
        return children;
    }

    public void setChildren(Set<OWLClassZChild> children) {
        this.children = children;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        OWLClassZChild that = (OWLClassZChild) o;
        return Objects.equals(id, that.id);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id);
    }

    @Override
    public String toString() {
        return "OWLClassZChild{<" + getId() + ">" +
                ", name='" + name + '\'' +
                '}';
    }
}

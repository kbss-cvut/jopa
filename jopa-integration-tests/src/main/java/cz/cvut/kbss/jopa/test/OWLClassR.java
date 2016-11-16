package cz.cvut.kbss.jopa.test;

import cz.cvut.kbss.jopa.CommonVocabulary;
import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.OWLAnnotationProperty;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;

import java.net.URI;

@OWLClass(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassR")
public class OWLClassR {

    @Id
    private URI uri;

    @OWLAnnotationProperty(iri = CommonVocabulary.RDFS_LABEL)
    private String name;

    public URI getUri() {
        return uri;
    }

    public void setUri(URI uri) {
        this.uri = uri;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) {
            return false;
        }

        OWLClassR owlClassR = (OWLClassR) o;

        return name != null ? name.equals(owlClassR.name) : owlClassR.name == null;

    }

    @Override
    public int hashCode() {
        return name != null ? name.hashCode() : 0;
    }

    @Override
    public String toString() {
        return "OWLClassR{" +
                "uri=" + uri +
                ", name='" + name + '\'' +
                '}';
    }
}

package cz.cvut.kbss.jopa.test;

import cz.cvut.kbss.jopa.model.annotations.*;

import java.net.URI;
import java.net.URL;
import java.util.List;
import java.util.Map;
import java.util.Set;

@OWLClass(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassP")
public class OWLClassP {

    @Id(generated = true)
    private URI uri;

    @OWLObjectProperty(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#hasIndividual")
    private URI individualUri;

    @OWLObjectProperty(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#hasOtherIndividual")
    private Set<URL> individuals;

    @Sequence(type = SequenceType.simple)
    @OWLObjectProperty(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#P-hasSimpleSequence")
    private List<URI> simpleList;

    @Sequence
    @OWLObjectProperty(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#P-hasReferencedSequence")
    private List<URI> referencedList;

    @Properties(fetchType = FetchType.EAGER)
    private Map<URI, Set<Object>> properties;

    public URI getUri() {
        return uri;
    }

    public void setUri(URI uri) {
        this.uri = uri;
    }

    public URI getIndividualUri() {
        return individualUri;
    }

    public void setIndividualUri(URI individualUri) {
        this.individualUri = individualUri;
    }

    public Set<URL> getIndividuals() {
        return individuals;
    }

    public void setIndividuals(Set<URL> individuals) {
        this.individuals = individuals;
    }

    public List<URI> getSimpleList() {
        return simpleList;
    }

    public void setSimpleList(List<URI> simpleList) {
        this.simpleList = simpleList;
    }

    public List<URI> getReferencedList() {
        return referencedList;
    }

    public void setReferencedList(List<URI> referencedList) {
        this.referencedList = referencedList;
    }

    public Map<URI, Set<Object>> getProperties() {
        return properties;
    }

    public void setProperties(Map<URI, Set<Object>> properties) {
        this.properties = properties;
    }

    @Override
    public String toString() {
        return "OWLClassP{" +
                "uri=" + uri +
                ", properties=" + properties +
                '}';
    }
}

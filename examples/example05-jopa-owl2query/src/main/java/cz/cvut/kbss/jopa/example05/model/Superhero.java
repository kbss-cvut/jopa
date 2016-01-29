package cz.cvut.kbss.jopa.example05.model;

import cz.cvut.kbss.jopa.model.annotations.*;

import java.io.Serializable;
import java.net.URI;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

@OWLClass(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/example05#Superhero")
public class Superhero implements Serializable {

    @Id
    private URI uri;

    @OWLDataProperty(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/example05#firstName")
    private String firstName;

    @OWLDataProperty(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/example05#lastName")
    private String lastName;

    @ParticipationConstraints(nonEmpty = true)
    @OWLDataProperty(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/example05#nickname")
    private String nickname;

    @OWLObjectProperty(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/example05#knows")
    private Set<Superhero> associates;

    @Properties
    private Map<String, Set<String>> properties;

    public Superhero() {
    }

    public Superhero(String nickname) {
        this.nickname = nickname;
    }

    public URI getUri() {
        return uri;
    }

    public void setUri(URI uri) {
        this.uri = uri;
    }

    public void generateUri() {
        if (uri == null) {
            assert nickname != null;
            this.uri = URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/example05#" + nickname.replace(' ', '_'));
        }
    }

    public String getFirstName() {
        return firstName;
    }

    public void setFirstName(String firstName) {
        this.firstName = firstName;
    }

    public String getLastName() {
        return lastName;
    }

    public void setLastName(String lastName) {
        this.lastName = lastName;
    }

    public String getNickname() {
        return nickname;
    }

    public void setNickname(String nickname) {
        this.nickname = nickname;
    }

    public Set<Superhero> getAssociates() {
        if (associates == null) {
            this.associates = new HashSet<>();
        }
        return associates;
    }

    public void setAssociates(Set<Superhero> associates) {
        this.associates = associates;
    }

    public void addAssociate(Superhero associate) {
        assert associate != null;
        getAssociates().add(associate);
    }

    public Map<String, Set<String>> getProperties() {
        return properties;
    }

    public void setProperties(Map<String, Set<String>> properties) {
        this.properties = properties;
    }

    @Override
    public String toString() {
        String result = nickname;
        if (firstName != null || lastName != null) {
            result += " (" + firstName + " " + lastName + ")";
        }
        return result;
    }
}

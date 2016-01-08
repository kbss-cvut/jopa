package cz.cvut.kbss.jopa.example02.model;

import cz.cvut.kbss.jopa.model.annotations.*;

import java.io.Serializable;
import java.net.URI;
import java.util.HashSet;
import java.util.Set;

@OWLClass(iri = Vocabulary.JEDI)
public class Jedi implements Serializable {

    @Id
    private URI uri;

    @ParticipationConstraints(nonEmpty = true)      // A value is required for this property
    @OWLDataProperty(iri = Vocabulary.FIRST_NAME)
    private String firstName;

    @ParticipationConstraints({@ParticipationConstraint(owlObjectIRI = Vocabulary.JEDI, min = 1, max = 1)})
    @OWLDataProperty(iri = Vocabulary.LAST_NAME)
    private String lastName;

    @OWLDataProperty(iri = Vocabulary.NICKNAME)
    private String nickname;

    @Inferred                                       // Inferred field is effectively read only
    @OWLObjectProperty(iri = Vocabulary.HAS_FATHER)
    private Jedi father;

    @OWLObjectProperty(iri = Vocabulary.HAS_CHILD, cascade = CascadeType.ALL)
    private Set<Jedi> children;

    public Jedi() {
    }

    public Jedi(String firstName, String lastName) {
        this.firstName = firstName;
        this.lastName = lastName;
        generateUri();
    }

    private void generateUri() {
        assert firstName != null;
        assert lastName != null;
        this.uri = URI.create(Vocabulary.BASE_URI + firstName + "+" + lastName);
    }

    public URI getUri() {
        return uri;
    }

    public void setUri(URI uri) {
        this.uri = uri;
    }

    public String getFirstName() {
        return firstName;
    }

    public void setFirstName(String firstName) {
        this.firstName = firstName;
        generateUri();
    }

    public String getLastName() {
        return lastName;
    }

    public void setLastName(String lastName) {
        this.lastName = lastName;
        generateUri();
    }

    public String getNickname() {
        return nickname;
    }

    public void setNickname(String nickname) {
        this.nickname = nickname;
    }

    public Jedi getFather() {
        return father;
    }

    public void setFather(Jedi father) {
        this.father = father;
    }

    public Set<Jedi> getChildren() {
        if (children == null) {
            children = new HashSet<>();
        }
        return children;
    }

    public void setChildren(Set<Jedi> children) {
        this.children = children;
    }

    public void addChild(Jedi jedi) {
        getChildren().add(jedi);
    }

    @Override
    public String toString() {
        String value = firstName + " " + lastName + " <" + uri + ">";
        if (nickname != null) {
            return nickname + " - " + value;
        }
        return value;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Jedi jedi = (Jedi) o;

        return !(uri != null ? !uri.equals(jedi.uri) : jedi.uri != null);

    }

    @Override
    public int hashCode() {
        return uri != null ? uri.hashCode() : 0;
    }
}

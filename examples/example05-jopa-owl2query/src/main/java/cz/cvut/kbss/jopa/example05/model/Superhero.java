/**
 * Copyright (C) 2011 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.example05.model;

import cz.cvut.kbss.jopa.model.annotations.*;
import cz.cvut.kbss.jopa.model.annotations.Properties;

import java.io.Serializable;
import java.net.URI;
import java.util.*;

@OWLClass(iri = Vocabulary.Superhero)
public class Superhero implements Serializable {

    @Id
    private URI uri;

    @OWLDataProperty(iri = Vocabulary.p_firstName)
    private String firstName;

    @OWLDataProperty(iri = Vocabulary.p_lastName)
    private String lastName;

    @ParticipationConstraints(nonEmpty = true)
    @OWLDataProperty(iri = Vocabulary.p_nickname)
    private String nickname;

    @OWLObjectProperty(iri = Vocabulary.p_knows)
    private Set<Superhero> associates;

    /**
     * Contains properties not mapped by the object model.
     */
    @Properties
    private Map<String, Set<String>> properties;

    public Superhero() {
    }

    public Superhero(String nickname) {
        this.nickname = nickname;
        generateUri();
    }

    public URI getUri() {
        return uri;
    }

    public void setUri(URI uri) {
        Objects.requireNonNull(uri);
        this.uri = uri;
    }

    private void generateUri() {
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

    public void addPropertyValue(String property, String value) {
        if (getProperties() == null) {
            this.properties = new HashMap<>();
        }
        if (!properties.containsKey(property)) {
            properties.put(property, new HashSet<>());
        }
        properties.get(property).add(value);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Superhero superhero = (Superhero) o;

        return uri.equals(superhero.uri);

    }

    @Override
    public int hashCode() {
        return uri.hashCode();
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

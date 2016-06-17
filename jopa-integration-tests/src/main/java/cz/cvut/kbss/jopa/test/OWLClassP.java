/**
 * Copyright (C) 2016 Czech Technical University in Prague
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

    @Types
    private Set<URI> types;

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

    public Set<URI> getTypes() {
        return types;
    }

    public void setTypes(Set<URI> types) {
        this.types = types;
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
                ", types=" + types +
                ", properties=" + properties +
                '}';
    }
}

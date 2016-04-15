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
package cz.cvut.kbss.jopa.environment;

import cz.cvut.kbss.jopa.model.annotations.*;

import java.lang.reflect.Field;
import java.net.URI;
import java.net.URL;
import java.util.List;
import java.util.Map;
import java.util.Set;

@OWLClass(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassP")
public class OWLClassP {

    @Id(generated = true)
    private URI uri;

    @OWLObjectProperty(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#hasA")
    private URI individualUri;

    @OWLObjectProperty(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#hasIndividual")
    private Set<URL> individualUrls;

    @Sequence(type = SequenceType.simple)
    @OWLObjectProperty(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#P-hasSimpleSequence")
    private List<URI> simpleList;

    @Sequence
    @OWLObjectProperty(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#P-hasReferencedSequence")
    private List<URI> referencedList;

    @Properties
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

    public Set<URL> getIndividualUrls() {
        return individualUrls;
    }

    public void setIndividualUrls(Set<URL> individualUrls) {
        this.individualUrls = individualUrls;
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

    public static String getClassIri() {
        return OWLClassP.class.getAnnotation(OWLClass.class).iri();
    }

    public static Field getUriField() throws Exception {
        return OWLClassP.class.getDeclaredField("uri");
    }

    public static Field getIndividualUriField() throws Exception {
        return OWLClassP.class.getDeclaredField("individualUri");
    }

    public static Field getIndividualUrlsField() throws Exception {
        return OWLClassP.class.getDeclaredField("individualUrls");
    }

    public static Field getPropertiesField() throws Exception {
        return OWLClassP.class.getDeclaredField("properties");
    }

    public static Field getSimpleListField() throws Exception {
        return OWLClassP.class.getDeclaredField("simpleList");
    }

    public static Field getReferencedListField() throws Exception {
        return OWLClassP.class.getDeclaredField("referencedList");
    }
}

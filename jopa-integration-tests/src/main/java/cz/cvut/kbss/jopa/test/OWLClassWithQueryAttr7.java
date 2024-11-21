/*
 * JOPA
 * Copyright (C) 2024 Czech Technical University in Prague
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */
package cz.cvut.kbss.jopa.test;

import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.Sparql;

import java.net.URI;
import java.util.Collection;
import java.util.List;
import java.util.Set;

@OWLClass(iri = Vocabulary.C_OwlClassWithQueryAttr6)
public class OWLClassWithQueryAttr7 implements HasUri {

    private static final String QUERY = "SELECT ?x WHERE { VALUES (?x) {(99)(2)(99)}}";

    @Id
    private URI uri;

    @Sparql(query=QUERY)
    private Collection<Integer> collectionQueryAttribute;

    @Sparql(query=QUERY)
    private Set<Integer> setQueryAttribute;

    @Sparql(query=QUERY)
    private List<Integer> listQueryAttribute;

    public OWLClassWithQueryAttr7() {
    }

    public OWLClassWithQueryAttr7(URI uri) {
        this.uri = uri;
    }

    public void setUri(URI uri) {
        this.uri = uri;
    }

    @Override
    public URI getUri() {
        return uri;
    }

    public Collection<Integer> getCollectionQueryAttribute() {
        return collectionQueryAttribute;
    }

    public void setCollectionQueryAttribute(Collection<Integer> collectionQueryAttribute) {
        this.collectionQueryAttribute = collectionQueryAttribute;
    }

    public Set<Integer> getSetQueryAttribute() {
        return setQueryAttribute;
    }

    public void setSetQueryAttribute(Set<Integer> setQueryAttribute) {
        this.setQueryAttribute = setQueryAttribute;
    }

    public List<Integer> getListQueryAttribute() {
        return listQueryAttribute;
    }

    public void setListQueryAttribute(List<Integer> listQueryAttribute) {
        this.listQueryAttribute = listQueryAttribute;
    }

    public static String getSparqlQuery() {
        return QUERY;
    }

    @Override
    public String toString() {
        return "OWLClassWithQueryAttr7{" +
                "uri=" + uri +
                ", collectionQueryAttribute=" + collectionQueryAttribute +
                ", setQueryAttribute=" + setQueryAttribute +
                ", listQueryAttribute=" + listQueryAttribute +
                '}';
    }
}

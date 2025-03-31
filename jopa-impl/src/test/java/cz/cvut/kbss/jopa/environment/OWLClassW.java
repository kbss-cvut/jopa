/*
 * JOPA
 * Copyright (C) 2025 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.environment;

import cz.cvut.kbss.jopa.model.annotations.FetchType;
import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;
import cz.cvut.kbss.jopa.model.annotations.Sequence;
import cz.cvut.kbss.jopa.model.annotations.Sparql;

import java.io.Serializable;
import java.lang.reflect.Field;
import java.net.URI;
import java.util.Collection;
import java.util.List;
import java.util.Set;

@OWLClass(iri = Vocabulary.c_OwlClassW)
public class OWLClassW implements Serializable {

    @Id(generated = true)
    private URI id;

    @OWLDataProperty(iri = Vocabulary.P_W_SET_STRING_ATTRIBUTE)
    private Set<String> setStringAtt;

    @Sequence
    @OWLDataProperty(iri = Vocabulary.P_W_LIST_STRING_ATTRIBUTE)
    private List<String> listStringAtt;

    @OWLDataProperty(iri = Vocabulary.P_W_COLLECTION_STRING_ATTRIBUTE)
    private Collection<String> collectionStringAtt;

    @Sparql(query = "SELECT ?x WHERE {?x a <" + Vocabulary.c_OwlClassW + "}", fetchType = FetchType.LAZY)
    private Set<String> setQueryStringAtt;

    @Sparql(query = "SELECT ?x WHERE {?x a <" + Vocabulary.c_OwlClassW + "}", fetchType = FetchType.LAZY)
    private List<String> listQueryStringAtt;

    public OWLClassW() {
    }

    public OWLClassW(URI id) {
        this.id = id;
    }

    public URI getId() {
        return id;
    }

    public void setId(URI id) {
        this.id = id;
    }

    public Collection<String> getSetStringAtt() {
        return setStringAtt;
    }

    public void setSetStringAtt(Set<String> setStringAtt) {
        this.setStringAtt = setStringAtt;
    }

    public Collection<String> getListStringAtt() {
        return listStringAtt;
    }

    public void setListStringAtt(List<String> listStringAtt) {
        this.listStringAtt = listStringAtt;
    }

    public Collection<String> getCollectionStringAtt() {
        return collectionStringAtt;
    }

    public void setCollectionStringAtt(Collection<String> collectionStringAtt) {
        this.collectionStringAtt = collectionStringAtt;
    }

    public Set<String> getSetQueryStringAtt() {
        return setQueryStringAtt;
    }

    public void setSetQueryStringAtt(Set<String> setQueryStringAtt) {
        this.setQueryStringAtt = setQueryStringAtt;
    }

    public List<String> getListQueryStringAtt() {
        return listQueryStringAtt;
    }

    public void setListQueryStringAtt(List<String> listQueryStringAtt) {
        this.listQueryStringAtt = listQueryStringAtt;
    }

    @Override
    public String toString() {
        return "OWLClassW{" +
                "id=" + id +
                ", setStringAtt=" + setStringAtt +
                ", listStringAtt=" + listStringAtt +
                ", collectionStringAtt=" + collectionStringAtt +
                ", setQueryStringAtt=" + setQueryStringAtt +
                ", listQueryStringAtt=" + listQueryStringAtt +
                '}';
    }

    public static String getClassIri() {
        return OWLClassW.class.getAnnotation(OWLClass.class).iri();
    }

    public static Field getIdField() throws NoSuchFieldException {
        return OWLClassW.class.getDeclaredField("id");
    }

    public static Field getSetStringAttField() throws NoSuchFieldException {
        return OWLClassW.class.getDeclaredField("setStringAtt");
    }

    public static Field getListStringAttField() throws NoSuchFieldException {
        return OWLClassW.class.getDeclaredField("listStringAtt");
    }

    public static Field getCollectionStringAttField() throws NoSuchFieldException {
        return OWLClassW.class.getDeclaredField("collectionStringAtt");
    }

    public static Field getSetQueryStringAttField() throws NoSuchFieldException {
        return OWLClassW.class.getDeclaredField("setQueryStringAtt");
    }

    public static Field getListQueryStringAttField() throws NoSuchFieldException {
        return OWLClassW.class.getDeclaredField("listQueryStringAtt");
    }
}

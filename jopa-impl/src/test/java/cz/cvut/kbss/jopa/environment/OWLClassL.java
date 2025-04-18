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

import cz.cvut.kbss.jopa.environment.utils.HasUri;
import cz.cvut.kbss.jopa.model.annotations.*;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.List;
import java.util.Set;

@OWLClass(iri = Vocabulary.c_OwlClassL)
public class OWLClassL implements HasUri {

    @Id
    private URI uri;

    @Sequence(type = SequenceType.simple)
    @OWLObjectProperty(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#C-hasSimpleSequence")
    @ParticipationConstraints({
            @ParticipationConstraint(min = 1, owlObjectIRI = "http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassA")
    })
    private List<OWLClassA> simpleList;

    @Sequence(type = SequenceType.referenced)
    @OWLObjectProperty(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#C-hasReferencedSequence")
    @ParticipationConstraints({
            @ParticipationConstraint(max = 2, owlObjectIRI = "http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassA")
    })
    private List<OWLClassA> referencedList;

    @OWLObjectProperty(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#hasA")
    @ParticipationConstraints({
            @ParticipationConstraint(min = 1, max = 5, owlObjectIRI = "http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassA")
    })
    private Set<OWLClassA> set;

    @ParticipationConstraints(nonEmpty = true)
    @OWLObjectProperty(iri = Vocabulary.p_l_singleA, fetch = FetchType.EAGER)
    private OWLClassA singleA;

    public OWLClassL() {
    }

    public OWLClassL(URI uri) {
        this.uri = uri;
    }

    public URI getUri() {
        return uri;
    }

    public void setUri(URI uri) {
        this.uri = uri;
    }

    public List<OWLClassA> getSimpleList() {
        return simpleList;
    }

    public void setSimpleList(List<OWLClassA> simpleList) {
        this.simpleList = simpleList;
    }

    public List<OWLClassA> getReferencedList() {
        return referencedList;
    }

    public void setReferencedList(List<OWLClassA> referencedList) {
        this.referencedList = referencedList;
    }

    public Set<OWLClassA> getSet() {
        return set;
    }

    public void setSet(Set<OWLClassA> set) {
        this.set = set;
    }

    public OWLClassA getSingleA() {
        return singleA;
    }

    public void setSingleA(OWLClassA singleA) {
        this.singleA = singleA;
    }

    @Override
    public String toString() {
        return "OWLClassL{" +
                "uri=" + uri +
                ", simpleList=" + simpleList +
                ", referencedList=" + referencedList +
                ", set=" + set +
                '}';
    }

    public static String getClassIri() {
        return OWLClassL.class.getAnnotation(OWLClass.class).iri();
    }

    public static Field getReferencedListField() throws NoSuchFieldException {
        return OWLClassL.class.getDeclaredField("referencedList");
    }

    public static Field getSimpleListField() throws NoSuchFieldException {
        return OWLClassL.class.getDeclaredField("simpleList");
    }

    public static Field getSetField() throws NoSuchFieldException {
        return OWLClassL.class.getDeclaredField("set");
    }

    public static Field getSingleAField() throws NoSuchFieldException {
        return OWLClassL.class.getDeclaredField("singleA");
    }
}

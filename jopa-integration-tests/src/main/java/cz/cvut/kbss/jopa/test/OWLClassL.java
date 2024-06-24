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

import cz.cvut.kbss.jopa.model.annotations.*;

import java.net.URI;
import java.util.List;
import java.util.Set;

@OWLClass(iri = Vocabulary.C_OWL_CLASS_L)
public class OWLClassL implements HasUri {

    @Id
    private URI uri;

    @Sequence(type = SequenceType.simple)
    @OWLObjectProperty(iri = Vocabulary.p_l_simpleListAttribute, fetch = FetchType.LAZY)
    @ParticipationConstraints({
            @ParticipationConstraint(min = 1, owlObjectIRI = Vocabulary.C_OWL_CLASS_A)
    })
    private List<OWLClassA> simpleList;

    @Sequence(type = SequenceType.referenced)
    @OWLObjectProperty(iri = Vocabulary.p_l_referencedListAttribute, fetch = FetchType.LAZY)
    @ParticipationConstraints({
            @ParticipationConstraint(max = 2, owlObjectIRI = Vocabulary.C_OWL_CLASS_A)
    })
    private List<OWLClassA> referencedList;

    @OWLObjectProperty(iri = Vocabulary.p_l_aSetAttribute, fetch = FetchType.LAZY)
    @ParticipationConstraints({
            @ParticipationConstraint(min = 1, max = 5, owlObjectIRI = Vocabulary.C_OWL_CLASS_A)
    })
    private Set<OWLClassA> set;

    @OWLObjectProperty(iri = Vocabulary.p_l_singleOwlClassAAttribute, fetch = FetchType.LAZY)
    @ParticipationConstraints({
            @ParticipationConstraint(min = 1, owlObjectIRI = Vocabulary.C_OWL_CLASS_A)
    })
    private OWLClassA singleA;

    public OWLClassL() {
    }

    public OWLClassL(URI uri) {
        this.uri = uri;
    }

    @Override
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
}

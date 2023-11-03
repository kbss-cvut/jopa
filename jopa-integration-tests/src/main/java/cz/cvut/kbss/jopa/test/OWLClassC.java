/*
 * JOPA
 * Copyright (C) 2023 Czech Technical University in Prague
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

@OWLClass(iri = Vocabulary.C_OWL_CLASS_C)
public class OWLClassC implements HasUri {

    @Id
    private URI uri;

    @Sequence
    @OWLObjectProperty(iri = Vocabulary.P_HAS_REFERENCED_LIST, fetch = FetchType.EAGER)
    private List<OWLClassA> referencedList;

    @Sequence(type = SequenceType.simple)
    @OWLObjectProperty(iri = Vocabulary.P_HAS_SIMPLE_LIST)
    private List<OWLClassA> simpleList;

    public OWLClassC() {
    }

    public OWLClassC(URI uri) {
        this.uri = uri;
    }

    public void setUri(URI uri) {
        this.uri = uri;
    }

    @Override
    public URI getUri() {
        return uri;
    }

    public void setReferencedList(List<OWLClassA> list) {
        this.referencedList = list;
    }

    public List<OWLClassA> getReferencedList() {
        return referencedList;
    }

    public void setSimpleList(List<OWLClassA> simpleList) {
        this.simpleList = simpleList;
    }

    public List<OWLClassA> getSimpleList() {
        return simpleList;
    }

    @Override
    public String toString() {
        String out = "OWLClassC: uri = " + uri;
        if (referencedList != null) {
            out += ", referencedList = {" + referencedList + "}";
        }
        if (simpleList != null) {
            out += ", simpleList = {" + simpleList + "}";
        }
        return out;
    }
}

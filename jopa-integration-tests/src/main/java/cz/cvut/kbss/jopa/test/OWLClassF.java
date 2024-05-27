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

import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.Inferred;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;
import cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty;

import java.net.URI;
import java.util.Set;

@OWLClass(iri = Vocabulary.C_OWL_CLASS_F)
public class OWLClassF implements HasUri {

    @Id
    private URI uri;

    @Inferred
    @OWLDataProperty(iri = Vocabulary.P_F_STRING_ATTRIBUTE)
    private String secondStringAttribute;

    @OWLObjectProperty(iri = Vocabulary.P_F_HAS_SIMPLE_SET)
    private Set<OWLClassA> simpleSet;

    public OWLClassF() {
    }

    public OWLClassF(URI uri) {
        this.uri = uri;
    }

    @Override
    public URI getUri() {
        return uri;
    }

    public void setUri(URI uri) {
        this.uri = uri;
    }

    public String getSecondStringAttribute() {
        return secondStringAttribute;
    }

    public void setSecondStringAttribute(String secondStringAttribute) {
        this.secondStringAttribute = secondStringAttribute;
    }

    public Set<OWLClassA> getSimpleSet() {
        return simpleSet;
    }

    public void setSimpleSet(Set<OWLClassA> simpleSet) {
        this.simpleSet = simpleSet;
    }

    @Override
    public String toString() {
        String out = "OWLClassF: uri = " + uri;
        out += ", secondStringAttribute = " + secondStringAttribute;
        return out;
    }
}

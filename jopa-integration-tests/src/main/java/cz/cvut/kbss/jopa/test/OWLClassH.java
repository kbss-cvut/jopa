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

@OWLClass(iri = Vocabulary.C_OWL_CLASS_H)
public class OWLClassH implements HasUri {

    @Id
    private URI uri;

    @OWLObjectProperty(iri = Vocabulary.P_HAS_OWL_CLASS_A, fetch = FetchType.EAGER, cascade = CascadeType.ALL)
    // @ParticipationConstraints({
    // @ParticipationConstraint(owlObjectIRI="http://new.owl#OWLClassA", min=1,
    // max=1)
    // })
    private OWLClassA owlClassA;

    public OWLClassH() {
    }

    public OWLClassH(URI uri) {
        this.uri = uri;
    }

    public void setUri(URI uri) {
        this.uri = uri;
    }

    @Override
    public URI getUri() {
        return uri;
    }

    public void setOwlClassA(OWLClassA owlClassA) {
        this.owlClassA = owlClassA;
    }

    public OWLClassA getOwlClassA() {
        return owlClassA;
    }

    @Override
    public String toString() {
        return "OWLClassH{" +
                "uri=" + uri +
                ", owlClassA=" + owlClassA +
                '}';
    }
}

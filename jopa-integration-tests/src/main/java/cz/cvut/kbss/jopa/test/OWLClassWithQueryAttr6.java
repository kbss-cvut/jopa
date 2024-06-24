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

import cz.cvut.kbss.jopa.model.annotations.CascadeType;
import cz.cvut.kbss.jopa.model.annotations.FetchType;
import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty;
import cz.cvut.kbss.jopa.model.annotations.Sparql;

import java.net.URI;

@OWLClass(iri = Vocabulary.C_OwlClassWithQueryAttr6)
public class OWLClassWithQueryAttr6 implements HasUri {

    private static final String QUERY = "SELECT ?d WHERE { " +
            "?d a <" + Vocabulary.C_OWL_CLASS_D + "> ;" +
            "     <" + Vocabulary.P_HAS_OWL_CLASS_A + "> ?a . " +
            "?this <" + Vocabulary.P_HAS_OWL_CLASS_A + "> ?a . }";

    @Id
    private URI uri;

    @OWLObjectProperty(iri = Vocabulary.P_HAS_OWL_CLASS_A, cascade = CascadeType.ALL)
    private OWLClassA owlClassA;

    @Sparql(query = QUERY, fetchType = FetchType.LAZY)
    private OWLClassD lazyQueryAttribute;

    public OWLClassWithQueryAttr6() {
    }

    public OWLClassWithQueryAttr6(URI uri) {
        this.uri = uri;
    }

    public void setUri(URI uri) {
        this.uri = uri;
    }

    @Override
    public URI getUri() {
        return uri;
    }

    public OWLClassA getOwlClassA() {
        return owlClassA;
    }

    public void setOwlClassA(OWLClassA owlClassA) {
        this.owlClassA = owlClassA;
    }

    public OWLClassD getLazyQueryAttribute() {
        return lazyQueryAttribute;
    }

    public void setLazyQueryAttribute(OWLClassD lazyQueryAttribute) {
        this.lazyQueryAttribute = lazyQueryAttribute;
    }

    @Override
    public String toString() {
        String out = "OWLClassWithQueryAttr: uri = " + uri;
        out += ", owlClassA = " + owlClassA;
        return out;
    }
}

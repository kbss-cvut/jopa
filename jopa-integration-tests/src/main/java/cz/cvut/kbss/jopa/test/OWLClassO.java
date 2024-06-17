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
import java.util.Set;

@OWLClass(iri = Vocabulary.C_OWL_CLASS_O)
public class OWLClassO implements HasUri {

    @Id
    private URI uri;

    @OWLObjectProperty(iri = Vocabulary.P_O_SET_OF_E_ATTRIBUTE, cascade = {CascadeType.MERGE}, fetch = FetchType.EAGER)
    private Set<OWLClassE> owlClassESet;

    @OWLObjectProperty(iri = Vocabulary.P_O_SINGLE_E_ATTRIBUTE, cascade = CascadeType.ALL, fetch = FetchType.LAZY)
    private OWLClassE owlClassE;

    public OWLClassO() {
    }

    public OWLClassO(URI uri) {
        this.uri = uri;
    }

    @Override
    public URI getUri() {
        return uri;
    }

    public void setUri(URI uri) {
        this.uri = uri;
    }

    public Set<OWLClassE> getOwlClassESet() {
        return owlClassESet;
    }

    public void setOwlClassESet(Set<OWLClassE> owlClassESet) {
        this.owlClassESet = owlClassESet;
    }

    public OWLClassE getOwlClassE() {
        return owlClassE;
    }

    public void setOwlClassE(OWLClassE owlClassE) {
        this.owlClassE = owlClassE;
    }

    @Override
    public String toString() {
        return "OWLClassO{" +
                "uri=" + uri +
                ", owlClassESet=" + owlClassESet +
                ", owlClassE=" + owlClassE +
                '}';
    }
}

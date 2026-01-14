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

import cz.cvut.kbss.jopa.environment.listener.MappedSuperclassListener;
import cz.cvut.kbss.jopa.environment.utils.HasUri;
import cz.cvut.kbss.jopa.model.annotations.*;
import cz.cvut.kbss.jopa.vocabulary.RDFS;

import java.net.URI;

@EntityListeners(MappedSuperclassListener.class)
@MappedSuperclass
public class QMappedSuperclass implements HasUri {

    @Id(generated = true)
    private URI uri;

    @OWLAnnotationProperty(iri = RDFS.LABEL)
    private String label;

    @OWLDataProperty(iri = Vocabulary.p_q_parentStringAttribute)
    private String parentString;

    @ParticipationConstraints(nonEmpty = true)
    @OWLObjectProperty(iri = Vocabulary.p_h_hasA, fetch = FetchType.EAGER)
    private OWLClassA owlClassA;

    public URI getUri() {
        return uri;
    }

    public void setUri(URI uri) {
        this.uri = uri;
    }

    public String getLabel() {
        return label;
    }

    public void setLabel(String label) {
        this.label = label;
    }

    public String getParentString() {
        return parentString;
    }

    public void setParentString(String parentString) {
        this.parentString = parentString;
    }

    public OWLClassA getOwlClassA() {
        return owlClassA;
    }

    public void setOwlClassA(OWLClassA owlClassA) {
        this.owlClassA = owlClassA;
    }
}

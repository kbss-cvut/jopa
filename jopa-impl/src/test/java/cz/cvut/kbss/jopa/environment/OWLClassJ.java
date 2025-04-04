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
import java.util.Set;

@OWLClass(iri = Vocabulary.c_OwlClassJ)
public class OWLClassJ implements HasUri {

    private static final String CLS_A_FIELD = "owlClassA";

    @Id
    private URI uri;

    @ParticipationConstraints(nonEmpty = true)
    @OWLObjectProperty(iri = Vocabulary.P_HAS_A, fetch = FetchType.LAZY, cascade = CascadeType.ALL)
    private Set<OWLClassA> owlClassA;

    public OWLClassJ() {
    }

    public OWLClassJ(URI uri) {
        this.uri = uri;
    }

    /**
     * @param uri
     *            the uri to set
     */
    public void setUri(URI uri) {
        this.uri = uri;
    }

    /**
     * @return the uri
     */
    public URI getUri() {
        return uri;
    }

    public void setOwlClassA(Set<OWLClassA> owlClassA) {
        this.owlClassA = owlClassA;
    }

    public Set<OWLClassA> getOwlClassA() {
        return owlClassA;
    }

    public static String getClassIri() {
        return OWLClassJ.class.getAnnotation(OWLClass.class).iri();
    }

    public static Field getOwlClassAField() throws NoSuchFieldException, SecurityException {
        return OWLClassJ.class.getDeclaredField(CLS_A_FIELD);
    }
}

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

import java.lang.reflect.Field;
import java.net.URI;
import java.util.Objects;

import cz.cvut.kbss.jopa.environment.utils.HasUri;
import cz.cvut.kbss.jopa.model.annotations.CascadeType;
import cz.cvut.kbss.jopa.model.annotations.FetchType;
import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty;

@OWLClass(iri = Vocabulary.c_OwlClassH)
public class OWLClassH implements HasUri {

    private static final String CLS_A_FIELD = "owlClassA";
    private static final String CLS_G_FIELD = "owlClassG";

    @Id
    private URI uri;

    @OWLObjectProperty(iri = Vocabulary.p_h_hasA, fetch = FetchType.EAGER, cascade = CascadeType.ALL)
    private OWLClassA owlClassA;

    // Inverse property to hasH
    @OWLObjectProperty(iri = Vocabulary.p_h_hasG, fetch = FetchType.EAGER)
    private OWLClassG owlClassG;

    public OWLClassH() {
    }

    public OWLClassH(URI uri) {
        this.uri = uri;
    }

    /**
     * @param uri the uri to set
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

    public void setOwlClassA(OWLClassA owlClassA) {
        this.owlClassA = owlClassA;
    }

    public OWLClassA getOwlClassA() {
        return owlClassA;
    }

    public OWLClassG getOwlClassG() {
        return owlClassG;
    }

    @Override
    public boolean equals(Object o) {
        if (!(o instanceof OWLClassH owlClassH)) {
            return false;
        }
        return Objects.equals(uri, owlClassH.uri);
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(uri);
    }

    public void setOwlClassG(OWLClassG owlClassG) {
        this.owlClassG = owlClassG;
    }

    public static String getClassIri() {
        return OWLClassH.class.getAnnotation(OWLClass.class).iri();
    }

    public static Field getOwlClassAField() throws NoSuchFieldException, SecurityException {
        return OWLClassH.class.getDeclaredField(CLS_A_FIELD);
    }

    public static Field getOwlClassGField() throws NoSuchFieldException, SecurityException {
        return OWLClassH.class.getDeclaredField(CLS_G_FIELD);
    }
}

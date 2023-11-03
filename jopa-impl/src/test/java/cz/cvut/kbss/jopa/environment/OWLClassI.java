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
package cz.cvut.kbss.jopa.environment;

import cz.cvut.kbss.jopa.model.annotations.*;

import java.lang.reflect.Field;
import java.net.URI;

@OWLClass(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassI")
public class OWLClassI {

    private static final String CLS_A_FIELD = "owlClassA";

    @Id
    private URI uri;

    @OWLObjectProperty(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#hasA", fetch = FetchType.LAZY, cascade = CascadeType.PERSIST)
    private OWLClassA owlClassA;

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

    public void setOwlClassA(OWLClassA owlClassA) {
        this.owlClassA = owlClassA;
    }

    public OWLClassA getOwlClassA() {
        return owlClassA;
    }

    public static String getClassIri() {
        return OWLClassI.class.getAnnotation(OWLClass.class).iri();
    }

    public static Field getOwlClassAField() throws NoSuchFieldException, SecurityException {
        return OWLClassI.class.getDeclaredField(CLS_A_FIELD);
    }
}

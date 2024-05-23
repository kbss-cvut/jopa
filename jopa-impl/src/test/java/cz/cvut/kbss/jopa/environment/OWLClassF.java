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

import cz.cvut.kbss.jopa.environment.utils.HasUri;
import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.Inferred;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;
import cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.Set;

@OWLClass(iri = Vocabulary.c_OwlClassF)
public class OWLClassF implements HasUri {

    private static final String STR_ATT_FIELD = "secondStringAttribute";
    private static final String SET_FIELD = "simpleSet";

    @Id
    private URI uri;

    @Inferred
    @OWLDataProperty(iri = Vocabulary.p_f_stringAttribute)
    private String secondStringAttribute;

    @OWLObjectProperty(iri = Vocabulary.p_f_setAttribute)
    private Set<OWLClassA> simpleSet;

    public OWLClassF() {
    }

    public OWLClassF(URI uri) {
        this.uri = uri;
    }

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

    public static String getClassIri() {
        return OWLClassF.class.getAnnotation(OWLClass.class).iri();
    }

    public static Field getStrAttField() throws NoSuchFieldException, SecurityException {
        return OWLClassF.class.getDeclaredField(STR_ATT_FIELD);
    }

    public static Field getSimpleSetField() throws NoSuchFieldException, SecurityException {
        return OWLClassF.class.getDeclaredField(SET_FIELD);
    }

    @Override
    public String toString() {
        String out = "OWLClassF: uri = " + uri;
        out += ", secondStringAttribute = " + secondStringAttribute;
        return out;
    }
}

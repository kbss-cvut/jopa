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
package cz.cvut.kbss.jopa.environment;

import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;

import java.io.Serializable;
import java.lang.reflect.Field;
import java.net.URI;

@OWLClass(iri = Vocabulary.c_OwlClassV)
public class OWLClassV implements Serializable {

    @Id
    private URI id;

    @OWLDataProperty(iri = Vocabulary.P_V_SINGULAR_DYNAMIC_ATTRIBUTE)
    private Object singularDynamicAtt;

    @OWLDataProperty(iri = Vocabulary.P_V_PLURAL_DYNAMIC_ATTRIBUTE)
    private Object pluralDynamicAtt;

    public OWLClassV() {
    }

    public OWLClassV(URI id) {
        this.id = id;
    }

    public URI getId() {
        return id;
    }

    public void setId(URI id) {
        this.id = id;
    }

    public Object getSingularDynamicAtt() {
        return singularDynamicAtt;
    }

    public void setSingularDynamicAtt(Object singularDynamicAtt) {
        this.singularDynamicAtt = singularDynamicAtt;
    }

    public Object getPluralDynamicAtt() {
        return pluralDynamicAtt;
    }

    public void setPluralDynamicAtt(Object pluralDynamicAtt) {
        this.pluralDynamicAtt = pluralDynamicAtt;
    }

    @Override
    public String toString() {
        return "OWLClassU{" +
                "id=" + id +
                ", singularDynamicAtt=" + singularDynamicAtt +
                ", pluralDynamicAtt=" + pluralDynamicAtt +
                '}';
    }

    public static String getClassIri() {
        return OWLClassV.class.getAnnotation(OWLClass.class).iri();
    }

    public static Field getIdField() throws Exception {
        return OWLClassV.class.getDeclaredField("id");
    }

    public static Field getSingularDynamicAttField() throws Exception {
        return OWLClassV.class.getDeclaredField("singularDynamicAtt");
    }

    public static Field getPluralDynamicAttField() throws Exception {
        return OWLClassV.class.getDeclaredField("pluralDynamicAtt");
    }
}


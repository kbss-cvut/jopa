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

import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;

import java.lang.reflect.Field;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

@OWLClass(iri = Vocabulary.c_OwlClassQ)
public class OWLClassQ extends QMappedSuperclass {

    @OWLDataProperty(iri = Vocabulary.p_q_stringAttribute)
    private String stringAttribute;

    public String getStringAttribute() {
        return stringAttribute;
    }

    public void setStringAttribute(String stringAttribute) {
        this.stringAttribute = stringAttribute;
    }

    public static String getClassIri() {
        return OWLClassQ.class.getAnnotation(OWLClass.class).iri();
    }

    public static Field getUriField() throws Exception {
        return QMappedSuperclass.class.getDeclaredField("uri");
    }

    public static Field getStringAttributeField() throws Exception {
        return OWLClassQ.class.getDeclaredField("stringAttribute");
    }

    public static Field getLabelField() throws Exception {
        return QMappedSuperclass.class.getDeclaredField("label");
    }

    public static Field getParentStringField() throws Exception {
        return QMappedSuperclass.class.getDeclaredField("parentString");
    }

    public static Field getOwlClassAField() throws Exception {
        return QMappedSuperclass.class.getDeclaredField("owlClassA");
    }

    public static Set<Field> getPersistentFields() throws Exception {
        return new HashSet<>(
                Arrays.asList(getStringAttributeField(), getParentStringField(), getLabelField(), getOwlClassAField()));
    }
}

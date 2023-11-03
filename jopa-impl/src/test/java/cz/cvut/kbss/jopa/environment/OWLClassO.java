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

import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;
import cz.cvut.kbss.jopa.model.annotations.Transient;

import java.lang.reflect.Field;
import java.net.URI;

@OWLClass(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassO")
public class OWLClassO {

    // Static fields should be transient
    private static boolean primitiveField = false;

    // Static fields should be transient
    private static final String STR_ATT_FIELD = "stringAttribute";

    public static final String TRANSIENT_ANNOTATED_FIELD_NAME = "transientFieldWithAnnotation";
    public static final String TRANSIENT_FIELD_NAME = "transientField";
    public static final String TRANSIENT_FINAL_FIELD_NAME = "finalField";

    @Id
    private URI uri;

    @OWLDataProperty(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#O-stringAttribute")
    private String stringAttribute;

    @Transient
    private String transientFieldWithAnnotation;

    private transient String transientField;    // Effectively transient

    private final String finalField = "I am final, therefore transient.";

    public OWLClassO() {
    }

    public OWLClassO(URI uri) {
        this.uri = uri;
    }

    public URI getUri() {
        return uri;
    }

    public void setUri(URI uri) {
        this.uri = uri;
    }

    public String getStringAttribute() {
        return stringAttribute;
    }

    public void setStringAttribute(String stringAttribute) {
        this.stringAttribute = stringAttribute;
    }

    public String getTransientFieldWithAnnotation() {
        return transientFieldWithAnnotation;
    }

    public void setTransientFieldWithAnnotation(String transientFieldWithAnnotation) {
        this.transientFieldWithAnnotation = transientFieldWithAnnotation;
    }

    public String getTransientField() {
        return transientField;
    }

    public void setTransientField(String transientField) {
        this.transientField = transientField;
    }

    public String getFinalField() {
        return finalField;
    }

    public static String getClassIri() {
        return OWLClassO.class.getAnnotation(OWLClass.class).iri();
    }

    public static Field getUriField() throws Exception {
        return OWLClassO.class.getDeclaredField("uri");
    }

    public static Field getStringAttributeField() throws Exception {
        return OWLClassO.class.getDeclaredField(STR_ATT_FIELD);
    }
}

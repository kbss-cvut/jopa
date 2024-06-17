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

import cz.cvut.kbss.jopa.model.annotations.*;
import cz.cvut.kbss.jopa.vocabulary.DC;

import java.net.URI;
import java.util.Map;
import java.util.Set;

@OWLClass(iri = Vocabulary.C_OWL_CLASS_N)
public class OWLClassN {

    @Id(generated = true)
    private String id;

    @OWLAnnotationProperty(iri = Vocabulary.P_N_STR_ANNOTATION_PROPERTY)
    private String annotationProperty;

    @OWLAnnotationProperty(iri = Vocabulary.P_N_URI_ANNOTATION_PROPERTY)
    private URI annotationUri;

    @ParticipationConstraints(nonEmpty = true)
    @OWLDataProperty(iri = Vocabulary.P_N_STRING_ATTRIBUTE)
    private String stringAttribute;

    @OWLAnnotationProperty(iri = DC.Terms.SOURCE)
    private Set<String> pluralAnnotationProperty;

    @Inferred
    @Properties(fetchType = FetchType.LAZY)
    private Map<String, Set<String>> properties;

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public String getAnnotationProperty() {
        return annotationProperty;
    }

    public void setAnnotationProperty(String annotationProperty) {
        this.annotationProperty = annotationProperty;
    }

    public URI getAnnotationUri() {
        return annotationUri;
    }

    public void setAnnotationUri(URI annotationUri) {
        this.annotationUri = annotationUri;
    }

    public String getStringAttribute() {
        return stringAttribute;
    }

    public void setStringAttribute(String stringAttribute) {
        this.stringAttribute = stringAttribute;
    }

    public Set<String> getPluralAnnotationProperty() {
        return pluralAnnotationProperty;
    }

    public void setPluralAnnotationProperty(Set<String> pluralAnnotationProperty) {
        this.pluralAnnotationProperty = pluralAnnotationProperty;
    }

    public Map<String, Set<String>> getProperties() {
        return properties;
    }

    public void setProperties(Map<String, Set<String>> properties) {
        this.properties = properties;
    }

    @Override
    public String toString() {
        return "OWLClassN{" +
                "id='" + id + '\'' +
                ", annotationProperty='" + annotationProperty + '\'' +
                ", annotationUri=" + annotationUri +
                ", stringAttribute='" + stringAttribute + '\'' +
                ", properties=" + properties +
                '}';
    }
}

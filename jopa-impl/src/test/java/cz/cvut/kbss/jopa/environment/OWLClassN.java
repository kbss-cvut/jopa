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
import cz.cvut.kbss.jopa.model.metamodel.PropertyInfo;
import cz.cvut.kbss.jopa.vocabulary.DC;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.Map;
import java.util.Set;

@OWLClass(iri = Vocabulary.c_OwlClassN)
public class OWLClassN {

    @Id(generated = true)
    private String id;

    @OWLAnnotationProperty(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#annotationProperty")
    private String annotationProperty;

    @OWLAnnotationProperty(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#annotationUri")
    private URI annotationUri;

    @ParticipationConstraints(nonEmpty = true)
    @OWLDataProperty(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#A-stringAttribute")
    private String stringAttribute;

    @OWLAnnotationProperty(iri = DC.Terms.SOURCE)
    private Set<String> pluralAnnotation;

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

    public Set<String> getPluralAnnotation() {
        return pluralAnnotation;
    }

    public void setPluralAnnotation(Set<String> pluralAnnotation) {
        this.pluralAnnotation = pluralAnnotation;
    }

    public Map<String, Set<String>> getProperties() {
        return properties;
    }

    public void setProperties(Map<String, Set<String>> properties) {
        this.properties = properties;
    }

    public static String getClassIri() {
        return OWLClassN.class.getAnnotation(OWLClass.class).iri();
    }

    public static Field getUriField() throws Exception {
        return OWLClassN.class.getDeclaredField("id");
    }

    public static Field getAnnotationPropertyField() throws Exception {
        return OWLClassN.class.getDeclaredField("annotationProperty");
    }
    public static PropertyInfo getAnnotationPropertyFieldInfo() throws Exception {
        return PropertyInfo.from(OWLClassN.getAnnotationPropertyField());
    }

    public static Field getAnnotationUriField() throws Exception {
        return OWLClassN.class.getDeclaredField("annotationUri");
    }

    public static Field getStringAttributeField() throws Exception {
        return OWLClassN.class.getDeclaredField("stringAttribute");
    }

    public static Field getPluralAnnotationField() throws Exception {
        return OWLClassN.class.getDeclaredField("pluralAnnotation");
    }

    public static Field getPropertiesField() throws Exception {
        return OWLClassN.class.getDeclaredField("properties");
    }
}

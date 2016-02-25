/**
 * Copyright (C) 2016 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.environment;

import cz.cvut.kbss.jopa.model.annotations.*;

import java.lang.reflect.Field;
import java.util.Map;
import java.util.Set;

@OWLClass(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassN")
public class OWLClassN {

    @Id(generated = true)
    private String id;

    @OWLAnnotationProperty(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#annotationProperty")
    private String annotationProperty;

    @ParticipationConstraints(nonEmpty = true)
    @OWLDataProperty(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#A-stringAttribute")
    private String stringAttribute;

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

    public String getStringAttribute() {
        return stringAttribute;
    }

    public void setStringAttribute(String stringAttribute) {
        this.stringAttribute = stringAttribute;
    }

    public Map<String, Set<String>> getProperties() {
        return properties;
    }

    public void setProperties(Map<String, Set<String>> properties) {
        this.properties = properties;
    }

    public static String getClassIri() throws Exception {
        return OWLClassN.class.getAnnotation(OWLClass.class).iri();
    }

    public static Field getAnnotationPropertyField() throws Exception {
        return OWLClassN.class.getDeclaredField("annotationProperty");
    }

    public static Field getStringAttributeField() throws Exception {
        return OWLClassN.class.getDeclaredField("stringAttribute");
    }

    public static Field getPropertiesField() throws Exception {
        return OWLClassN.class.getDeclaredField("properties");
    }
}

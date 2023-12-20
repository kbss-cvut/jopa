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
package cz.cvut.kbss.jopa.test;

import cz.cvut.kbss.jopa.model.annotations.*;

import java.net.URI;
import java.util.Map;
import java.util.Set;

@OWLClass(iri = Vocabulary.C_OWL_CLASS_B)
public class OWLClassB implements HasUri {

    @Id
    private URI uri;

    @OWLDataProperty(iri = Vocabulary.P_B_STRING_ATTRIBUTE)
    private String stringAttribute;

    @Properties(fetchType = FetchType.LAZY)
    private Map<String, Set<String>> properties;

    public void setUri(URI uri) {
        this.uri = uri;
    }

    @Override
    public URI getUri() {
        return uri;
    }

    public void setStringAttribute(String stringAttribute) {
        this.stringAttribute = stringAttribute;
    }

    public String getStringAttribute() {
        return stringAttribute;
    }

    public void setProperties(Map<String, Set<String>> properties) {
        this.properties = properties;
    }

    public Map<String, Set<String>> getProperties() {
        return properties;
    }

    @Override
    public String toString() {
        String out = "OWLClassB: uri = " + uri;
        out += ", stringAttribute = " + stringAttribute;
        return out;
    }
}

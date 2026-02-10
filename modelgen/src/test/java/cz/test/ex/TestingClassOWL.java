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
package cz.test.ex;

import cz.cvut.kbss.jopa.model.annotations.FetchType;
import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.Inferred;
import cz.cvut.kbss.jopa.model.annotations.OWLAnnotationProperty;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;
import cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty;
import cz.cvut.kbss.jopa.model.annotations.Properties;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.List;
import java.util.Map;
import java.util.Set;

@OWLClass(iri="http://krizik.felk.cvut.cz/ontologies/jopa/classes/TestingClassOWL")
public class TestingClassOWL {
    @Id(generated = true)
    private URI uri;

    @CustomAnnotation
    @OWLObjectProperty(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#objectProperty")
    private TestingClassOWL testingClassOWL;

    @Inferred
    @Properties(fetchType = FetchType.LAZY)
    private Map<String, Set<String>> properties;
    @Properties
    private Map<URI, Set<Object>> propertie;
    @OWLDataProperty(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#stringList")
    private List<String> listAttribute;
    @OWLAnnotationProperty(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#stringSet")
    private Set<String> setAttribute;

    public URI getUri() {
        return uri;
    }

    public void setUri(URI uri) {
        this.uri = uri;
    }

    public static Field field(String name) {
        try {
            return TestingClassOWL.class.getDeclaredField(name);
        } catch (NoSuchFieldException e) {
            throw new RuntimeException(e);
        }
    }
}

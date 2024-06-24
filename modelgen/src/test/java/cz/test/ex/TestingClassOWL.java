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
package cz.test.ex;

import cz.cvut.kbss.jopa.model.annotations.*;

import java.net.URI;
import java.util.List;
import java.util.Map;
import java.util.Set;

@OWLClass(iri="")
public class TestingClassOWL {
    @Id(generated = true)
    private URI uri;

    @OWLDataProperty(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#E-stringAttribute")
    private TestingClassOWL testingClassOWL;

    @Inferred
    @Properties(fetchType = FetchType.LAZY)
    private Map<String, Set<String>> properties;
    @Properties
    private Map<URI, Set<Object>> propertie;
    @OWLObjectProperty(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#A-stringAttribute")
    private List<String> listAttribute;
    @OWLObjectProperty(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#A-stringAttribute")
    private Set<String> setAttribute;

//    @Types
//    private Set rawTypes;

    public URI getUri() {
        return uri;
    }

    public void setUri(URI uri) {
        this.uri = uri;
    }

}
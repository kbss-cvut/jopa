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
package cz.cvut.kbss.jopa.test;

import cz.cvut.kbss.jopa.model.annotations.FetchType;
import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.MappedSuperclass;
import cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty;

import java.net.URI;
import java.util.Set;

@MappedSuperclass
public abstract class AbstractGenericClass<T extends HasUri> {

    @Id
    private URI uri;

    @OWLObjectProperty(iri = Vocabulary.ATTRIBUTE_IRI_BASE + "genericValue", fetch = FetchType.EAGER)
    private Set<T> genericValue;

    public URI getUri() {
        return uri;
    }

    public void setUri(URI uri) {
        this.uri = uri;
    }

    public Set<T> getGenericValue() {
        return genericValue;
    }

    public void setGenericValue(Set<T> genericValue) {
        this.genericValue = genericValue;
    }
}

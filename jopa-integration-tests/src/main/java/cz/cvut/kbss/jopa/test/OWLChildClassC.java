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

import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;

import java.net.URI;
import java.util.Set;

@OWLClass(iri = Vocabulary.C_OWL_CLASS_CHILD_C)
public class OWLChildClassC implements OWLInterfaceC, OWLInterfaceD {
    private String name;

    private Boolean attributeB;

    Set<String> titles;
    @Id
    private URI id;

    public URI getId() {
        return id;
    }

    public void setId(URI id) {
        this.id = id;
    }

    @Override
    public void setName(String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }

    @Override
    public void setAttributeB(Boolean attr) {
        this.attributeB = attr;
    }


    public Boolean getAttributeB() {
        return attributeB;
    }

    @Override
    public Set<String> getTitles() {
        return titles;
    }

    public void setTitles(Set<String> titles) {
        this.titles = titles;
    }
}


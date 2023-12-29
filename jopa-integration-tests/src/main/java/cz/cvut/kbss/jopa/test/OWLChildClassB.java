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

import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;

import java.net.URI;

@OWLClass(iri = Vocabulary.C_OWL_CLASS_CHILD_B)
public class OWLChildClassB implements OWLInterfaceA, OWLInterfaceB {
    private String attributeA;

    private Boolean attributeB;
    @Id
    private URI id;

    public URI getId() {
        return id;
    }

    public void setId(URI id) {
        this.id = id;
    }

    @Override
    public String getAttributeA() {
        return attributeA;
    }

    @Override
    public void setAttributeB(Boolean attr) {
        attributeB = attr;
    }

    @Override
    public void setAttributeA(String attributeA) {
        this.attributeA = attributeA;
    }

    @Override
    public Boolean getAttributeB() {
        return attributeB;
    }
}

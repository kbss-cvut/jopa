/*
 * JOPA
 * Copyright (C) 2026 Czech Technical University in Prague
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
import cz.cvut.kbss.jopa.utils.IdentifierTransformer;

import java.net.URI;

@OWLClass(iri = Vocabulary.c_Phone)
public class Phone {

    @Id
    private URI uri;

    @OWLDataProperty(iri = Vocabulary.p_p_phoneNumber)
    private String number;

    @OWLDataProperty(iri = Vocabulary.p_p_phoneBrand, simpleLiteral = true)
    private String brand;

    public Phone() {
    }

    public Phone(URI uri, String number, String brand) {
        this.uri = uri;
        this.number = number;
        this.brand = brand;
    }

    public URI getUri() {
        return uri;
    }

    public void setUri(URI uri) {
        this.uri = uri;
    }

    public String getNumber() {
        return number;
    }

    public void setNumber(String number) {
        this.number = number;
    }

    public String getBrand() {
        return brand;
    }

    public void setBrand(String brand) {
        this.brand = brand;
    }

    @Override
    public String toString() {
        return "Phone{" +
                IdentifierTransformer.stringifyIri(uri) +
                ", number='" + number + '\'' +
                ", brand='" + brand + '\'' +
                '}';
    }
}
